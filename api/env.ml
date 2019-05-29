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

type env_error =
  | EnvErrorType        of typing_error
  | EnvErrorSignature   of signature_error
  | NonLinearRule       of name
  | NotEnoughArguments  of ident * int * int * int
  | KindLevelDefinition of ident
  | ParseError          of string
  | AssertError

exception EnvError of loc * env_error

let raise_as_env lc = function
  | SignatureError e -> raise (EnvError (lc, (EnvErrorSignature e)))
  | TypingError    e -> raise (EnvError (lc, (EnvErrorType      e)))
  | ex -> raise ex


(* Wrapper around Signature *)

let sg = ref (Signature.make "noname")

let check_arity     = ref true

let init file =
  sg := Signature.make file;
  Signature.get_name !sg

let get_name () = Signature.get_name !sg

let get_signature () = !sg

let get_type lc cst =
  try Signature.get_type !sg lc cst
  with e -> raise_as_env lc e

let get_dtree lc cst =
  try Signature.get_dtree !sg None lc cst
  with e -> raise_as_env lc e

let export () =
  try Signature.export !sg
  with e -> raise_as_env dloc e

let import lc md =
  try Signature.import !sg lc md
  with e -> raise_as_env lc e

let _declare lc (id:ident) scope st ty : unit =
  match inference !sg ty with
  | Kind | Type _ -> Signature.add_declaration !sg lc id scope st ty
  | s -> raise (TypingError (SortExpected (ty,[],s)))

let is_static lc cst = Signature.is_static !sg lc cst


(*         Rule checking       *)

(** Checks that all Miller variables are applied to the same number of
    distinct free variable on the left hand side.
    Checks that they are applied to at least as many arguments on the rhs.  *)
let _check_arity (r:rule_infos) : unit =
  let check l id n k nargs =
    let expected_args = r.arity.(n-k) in
    if nargs < expected_args
    then raise (EnvError (l, NotEnoughArguments (id,n,nargs,expected_args))) in
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

let _add_rules rs =
  let ris = List.map Rule.to_rule_infos rs in
  if !check_arity then List.iter _check_arity ris;
  Signature.add_rules !sg ris

let _define lc (id:ident) (scope : scope) (opaque:bool) (te:term) (ty_opt:typ option) : unit =
  let ty = match ty_opt with
    | None -> inference !sg te
    | Some ty -> ( checking !sg te ty; ty )
  in
  match ty with
  | Kind -> raise (EnvError (lc, KindLevelDefinition id))
  | _ ->
     if opaque
     then Signature.add_declaration !sg lc id scope Signature.Static ty
     else
       begin
         Signature.add_declaration !sg lc id scope Signature.Definable ty;
         let cst = mk_name (get_name ()) id in
         let rule =
           { name= Delta(cst) ;
             ctx = [] ;
             pat = Pattern(lc, cst, []);
             rhs = te ;
           }
         in
         _add_rules [rule]
       end

let declare lc id scope st ty : unit =
  try _declare lc id scope st ty
  with e -> raise_as_env lc e

let define lc id scope op te ty_opt : unit =
  try _define lc id scope op te ty_opt
  with e -> raise_as_env lc e

let add_rules (rules: untyped_rule list) : (Subst.Subst.t * typed_rule) list =
  try
    let rs2 = List.map (check_rule !sg) rules in
    _add_rules rules;
    rs2
  with e -> raise_as_env (get_loc_rule (List.hd rules)) e

let infer ?ctx:(ctx=[]) te =
  try
    let ty = infer !sg ctx te in
    ignore(infer !sg ctx ty);
    ty
  with e -> raise_as_env (get_loc te) e

let check ?ctx:(ctx=[]) te ty =
  try check !sg ctx te ty
  with e -> raise_as_env (get_loc te) e

let _unsafe_reduction red te =
  Reduction.reduction red !sg te

let _reduction ctx red te =
  ignore(Typing.infer !sg ctx te);
  _unsafe_reduction red te

let reduction ?ctx:(ctx=[]) ?red:(red=Reduction.default_cfg) te =
  try _reduction ctx red te
  with e -> raise_as_env (get_loc te) e

let unsafe_reduction ?red:(red=Reduction.default_cfg) te =
  try _unsafe_reduction red te
  with e -> raise_as_env (get_loc te) e

let are_convertible ?ctx:(ctx=[]) te1 te2 =
  try
    let ty1 = Typing.infer !sg ctx te1 in
    let ty2 = Typing.infer !sg ctx te2 in
    Reduction.are_convertible !sg ty1 ty2 &&
    Reduction.are_convertible !sg te1 te2
  with e -> raise_as_env (get_loc te1) e
