open Basic
open Term
open Rule
open Typing
open Signature

exception DebugFlagNotRecognized of char

let set_debug_mode =
  String.iter (function
      | 'q' -> Debug.disable_flag Debug.D_warn
      | 'n' -> Debug.enable_flag  Debug.D_notice
      | 'o' -> Debug.enable_flag  Signature.D_module
      | 'c' -> Debug.enable_flag  Confluence.D_confluence
      | 'u' -> Debug.enable_flag  Typing.D_rule
      | 't' -> Debug.enable_flag  Typing.D_typeChecking
      | 'r' -> Debug.enable_flag  Reduction.D_reduce
      | 'm' -> Debug.enable_flag  Dtree.D_matching
      | c -> raise (DebugFlagNotRecognized c)
    )

type t =
  {
    input : Parser.t;
    sg    : Signature.t;
    red   : (module Reduction.S);
    typer : (module Typing.S)
  }

type env_error =
  | EnvErrorType        of typing_error
  | EnvErrorSignature   of signature_error
  | EnvErrorRule        of rule_error
  | EnvErrorDep         of Dep.dep_error
  | NonLinearRule       of rule_name
  | NotEnoughArguments  of ident * int * int * int
  | KindLevelDefinition of ident
  | ParseError          of string
  | BracketScopingError
  | AssertError
  | Misc                of exn
  | FailExportFile      of mident * string

exception Env_error of t option * loc * env_error

let raise_env env lc err = raise (Env_error (Some env, lc, err))

let raise_as_env env lc = function
  | SignatureError e -> raise_env env lc (EnvErrorSignature e)
  | TypingError    e -> raise_env env lc (EnvErrorType      e)
  | RuleError      e -> raise_env env lc (EnvErrorRule      e)
  | ex               -> raise ex

let check_arity = ref true

let check_ll = ref false

let init input =
  let sg =  Signature.make (Parser.md_of_input input) Dep.find_object_file in
  let red : (module Reduction.S) = (module Reduction.Default) in
  let typer : (module Typing.S) = (module Typing.Default) in
  {input; sg;red;typer}

let set_reduction_engine env (module R:Reduction.S) =
  let red = (module R:Reduction.S) in
  let typer = (module Typing.Make(R):Typing.S) in
  {env with red;typer}

let get_reduction_engine env = env.red

let get_name env = Signature.get_name env.sg

let get_input env = env.input

let get_signature env = env.sg

let get_printer env : (module Pp.Printer) =
  (module Pp.Make(struct let get_name () = get_name env end))

module HName = Hashtbl.Make(
  struct
    type t    = name
    let equal = name_eq
    let hash  = Hashtbl.hash
  end )

let get_symbols env =
  let table = HName.create 11 in
  Signature.iter_symbols (fun md id -> HName.add table (mk_name md id)) env.sg;
  table

let get_type env lc cst =
  try Signature.get_type env.sg lc cst
  with e -> raise_as_env env lc e

let get_dtree env lc cst =
  try Signature.get_dtree env.sg lc cst
  with e -> raise_as_env env lc e

let export env =
  let file = Dep.object_file_of_input env.input in
  let oc = open_out file in
  try Signature.export env.sg oc; close_out oc
  with
  | Signature.SignatureError (Signature.CouldNotExportModule e) ->
    raise @@ Env_error(Some env, dloc, FailExportFile(e,file))
  | e -> close_out oc; raise_as_env env dloc e

let import env lc md =
  try Signature.import env.sg lc md
  with e -> raise_as_env env lc e

let _declare env lc (id:ident) st ty : unit =
  let (module T) = env.typer in
  match T.inference env.sg ty with
  | Kind | Type _ -> Signature.add_declaration env.sg lc id st ty
  | s -> raise (Typing.TypingError (Typing.SortExpected (ty,[],s)))

let is_static env lc cst = Signature.is_static env.sg lc cst

(*         Rule checking       *)

(* Checks that all Miller variables are applied to at least
   as many arguments on the rhs as they are on the lhs (their arity). *)
let _check_arity env (r:rule_infos) : unit =
  let check l id n k nargs =
    let expected_args = r.arity.(n-k) in
    if nargs < expected_args
    then raise_env env l (NotEnoughArguments (id,n,nargs,expected_args)) in
  let rec aux k = function
    | Kind | Type _ | Const _ -> ()
    | DB (l,id,n) ->
      if n >= k then check l id n k 0
    | App(DB(l,id,n),a1,args) when n>=k ->
      check l id n k (List.length args + 1);
      List.iter (aux k) (a1::args)
    | App (f,a1,args) -> List.iter (aux k) (f::a1::args)
    | Lam (_,_,None,b) -> aux (k+1) b
    | Lam (_,_,Some a,b) | Pi (_,_,a,b) -> (aux k a;  aux (k+1) b)
  in
  aux 0 r.rhs

(** Checks that all rule are left-linear. *)
let _check_ll env (r:rule_infos) : unit =
  List.iter
    (function Linearity _ -> raise (Env_error (Some env, r.l, NonLinearRule r.name)) | _ -> ())
    r.constraints

let _add_rules env rs =
  let ris = List.map Rule.to_rule_infos rs in
  if !check_arity then List.iter (_check_arity env) ris;
  if !check_ll    then List.iter (_check_ll env)    ris;
  Signature.add_rules env.sg ris

let _define env lc (id:ident) (opaque:bool) (te:term) (ty_opt:Typing.typ option) : unit =
  let (module T) = env.typer in
  let ty = match ty_opt with
    | None -> T.inference env.sg te
    | Some ty -> T.checking env.sg te ty; ty
  in
  match ty with
  | Kind -> raise_env env lc (KindLevelDefinition id)
  | _ ->
    if opaque then Signature.add_declaration env.sg lc id Signature.Static ty
    else
      let _ = Signature.add_declaration env.sg lc id Signature.Definable ty in
      let cst = mk_name (get_name env) id in
      let rule =
        { name= Delta(cst) ;
          ctx = [] ;
          pat = Pattern(lc, cst, []);
          rhs = te ;
        }
      in
      _add_rules env [rule]

let declare env lc id st ty : unit =
  try _declare env lc id st ty
  with e -> raise_as_env env lc e

let define env lc id op te ty_opt : unit =
  try _define env lc id op te ty_opt
  with e -> raise_as_env env lc e

let add_rules env (rules: untyped_rule list) : (Subst.Subst.t * typed_rule) list =
  let (module T) = env.typer in
  try
    let rs2 = List.map (T.check_rule env.sg) rules in
    _add_rules env rules;
    rs2
  with e -> raise_as_env env (get_loc_rule (List.hd rules)) e

let infer env ?ctx:(ctx=[]) te =
  let (module T) = env.typer in
  try
    let ty = T.infer env.sg ctx te in
    (* We only verify that [ty] itself has a type (that we immediately
       throw away) if [ty] is not [Kind], because [Kind] does not have a
       type, but we still want [infer ctx Type] to produce [Kind] *)
    if ty <> mk_Kind then
      ignore(T.infer env.sg ctx ty);
    ty
  with e -> raise_as_env env (get_loc te) e

let check env ?ctx:(ctx=[]) te ty =
  let (module T) = env.typer in
  try T.check env.sg ctx te ty
  with e -> raise_as_env env (get_loc te) e

let _unsafe_reduction env red te =
  let (module R) = env.red in
  R.reduction red env.sg te

let _reduction env ctx red te =
  (* This is a safe reduction, so we check that [te] has a type
     before attempting to normalize it, but we only do so if [te]
     is not [Kind], because [Kind] does not have a type, but we
     still want to be able to reduce it *)
  let (module T) = env.typer in
  if te <> mk_Kind then
    ignore(T.infer env.sg ctx te);
  _unsafe_reduction env red te

let reduction env ?ctx:(ctx=[]) ?red:(red=Reduction.default_cfg) te =
  try _reduction env ctx red te
  with e -> raise_as_env env (get_loc te) e

let unsafe_reduction env ?red:(red=Reduction.default_cfg) te =
  try _unsafe_reduction env red te
  with e -> raise_as_env env (get_loc te) e

let are_convertible env ?ctx:(ctx=[]) te1 te2 =
  let (module T) = env.typer in
  let (module R) = env.red in
  try
    let ty1 = T.infer env.sg ctx te1 in
    let ty2 = T.infer env.sg ctx te2 in
    R.are_convertible env.sg ty1 ty2 &&
    R.are_convertible env.sg te1 te2
  with e -> raise_as_env env (get_loc te1) e
