open Basic
open Term
open Rule
open Typing
open Signature

type t = Signature.t

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

type env_error =
  | EnvErrorType        of typing_error
  | EnvErrorSignature   of signature_error
  | NonLinearRule       of name
  | NotEnoughArguments  of ident * int * int * int
  | KindLevelDefinition of ident
  | ParseError          of string
  | AssertError

exception EnvError of t * loc * env_error

let raise_as_env sg lc = function
  | SignatureError e -> raise (EnvError (sg, lc, (EnvErrorSignature e)))
  | TypingError    e -> raise (EnvError (sg, lc, (EnvErrorType      e)))
  | ex -> raise ex


(* Wrapper around Signature *)

let check_arity = ref true

let get_md = Signature.get_md

let init file =
  let sg = Signature.make file in
  let md = get_md sg in
  Pp.set_module md;
  sg

let get_signature sg = sg

let get_type sg lc cst =
  try Signature.get_type sg lc cst
  with e -> raise_as_env sg lc e

let get_dtree sg lc cst =
  try Signature.get_dtree sg None lc cst
  with e -> raise_as_env sg lc e

let export sg =
  try Signature.export sg
  with e -> raise_as_env sg dloc e

let import sg lc md =
  try Signature.import sg lc md
  with e -> raise_as_env sg lc e

let _declare sg lc (id:ident) st ty : unit =
  match inference sg ty with
  | Kind | Type _ -> Signature.add_declaration sg lc id st ty
  | s -> raise (TypingError (SortExpected (ty,[],s)))

let is_static sg lc cst = Signature.is_static sg lc cst


(*         Rule checking       *)

(** Checks that all Miller variables are applied to the same number of
    distinct free variable on the left hand side.
    Checks that they are applied to at least as many arguments on the rhs.  *)
let _check_arity sg (r:rule_infos) : unit =
  let check l id n k nargs =
    let expected_args = r.arity.(n-k) in
    if nargs < expected_args
    then raise (EnvError (sg, l, NotEnoughArguments (id,n,nargs,expected_args))) in
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

let _add_rules sg rs =
  let ris = List.map Rule.to_rule_infos rs in
  if !check_arity then List.iter (_check_arity sg) ris;
  Signature.add_rules sg ris

let _define sg lc (id:ident) (opaque:bool) (te:term) (ty_opt:typ option) : unit =
  let ty = match ty_opt with
    | None -> inference sg te
    | Some ty -> ( checking sg te ty; ty )
  in
  match ty with
  | Kind -> raise (EnvError (sg, lc, KindLevelDefinition id))
  | _ ->
    if opaque then Signature.add_declaration sg lc id Signature.Static ty
    else
      let _ = Signature.add_declaration sg lc id Signature.Definable ty in
      let cst = mk_name (get_md sg) id in
      let rule =
        { name= Delta(cst) ;
          ctx = [] ;
          pat = Pattern(lc, cst, []);
          rhs = te ;
        }
      in
      _add_rules sg [rule]

let declare sg lc id st ty : unit =
  try _declare sg lc id st ty
  with e -> raise_as_env sg lc e

let define sg lc id op te ty_opt : unit =
  try _define sg lc id op te ty_opt
  with e -> raise_as_env sg lc e

let add_rules sg (rules: untyped_rule list) : (Subst.Subst.t * typed_rule) list =
  try
    let rs2 = List.map (check_rule sg) rules in
    _add_rules sg rules;
    rs2
  with e -> raise_as_env sg (get_loc_rule (List.hd rules)) e

let infer sg ?ctx:(ctx=[]) te =
  try
    let ty = infer sg ctx te in
    ignore(infer sg ctx ty);
    ty
  with e -> raise_as_env sg (get_loc te) e

let check sg ?ctx:(ctx=[]) te ty =
  try check sg ctx te ty
  with e -> raise_as_env sg (get_loc te) e

let _unsafe_reduction sg red te =
  Reduction.reduction red sg te

let _reduction sg ctx red te =
  ignore(Typing.infer sg ctx te);
  _unsafe_reduction sg red te

let reduction sg ?ctx:(ctx=[]) ?red:(red=Reduction.default_cfg) te =
  try _reduction sg ctx red te
  with e -> raise_as_env sg (get_loc te) e

let unsafe_reduction sg ?red:(red=Reduction.default_cfg) te =
  try _unsafe_reduction sg red te
  with e -> raise_as_env sg (get_loc te) e

let are_convertible sg ?ctx:(ctx=[]) te1 te2 =
  try
    let ty1 = Typing.infer sg ctx te1 in
    let ty2 = Typing.infer sg ctx te2 in
    Reduction.are_convertible sg ty1 ty2 &&
    Reduction.are_convertible sg te1 te2
  with e -> raise_as_env sg (get_loc te1) e
