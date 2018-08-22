open Basic
open Term
open Rule
open Typing
open Signature

type env_error =
  | EnvErrorType        of typing_error
  | EnvErrorSignature   of signature_error
  | KindLevelDefinition of loc * ident
  | ParseError          of loc * string
  | AssertError         of loc

exception EnvError of env_error

exception DefineExn of loc*ident

let raise_as_env = function
  | SignatureError e -> raise (EnvError (EnvErrorSignature e) )
  | TypingError    e -> raise (EnvError (EnvErrorType      e) )
  | DefineExn (l,id) -> raise (EnvError (KindLevelDefinition (l,id)))
  | ex -> raise ex


(* Wrapper around Signature *)

let sg = ref (Signature.make "noname")

let init file =
  sg := Signature.make file;
  Signature.get_name !sg

let get_name () = Signature.get_name !sg

let get_signature () = !sg

let get_type ?(loc=dloc) cst =
  try Signature.get_type !sg loc cst
  with e -> raise_as_env e

let get_dtree l cst =
  try Signature.get_dtree !sg None l cst
  with e -> raise_as_env e

let export () =
  try Signature.export !sg
  with e -> raise_as_env e

let import lc md =
  try Signature.import !sg lc md
  with e -> raise_as_env e

let _declare (l:loc) (id:ident) st ty : unit =
  match inference !sg ty with
  | Kind | Type _ -> Signature.add_declaration !sg l id st ty
  | s -> raise (TypingError (SortExpected (ty,[],s)))

let is_static lc cst = Signature.is_static !sg lc cst

let _define (l:loc) (id:ident) (opaque:bool) (te:term) (ty_opt:typ option) : unit =
  let ty = match ty_opt with
    | None -> inference !sg te
    | Some ty -> ( checking !sg te ty; ty )
  in
  match ty with
  | Kind -> raise (DefineExn (l,id))
  | _ ->
    if opaque then
      Signature.add_declaration !sg l id Signature.Static ty
    else
      let _ = Signature.add_declaration !sg l id Signature.Definable ty in
      let cst = mk_name (get_name ()) id in
      let rule =
        { name= Delta(cst) ;
          ctx = [] ;
          pat = Pattern(l, cst, []);
          rhs = te ;
        }
      in
      Signature.add_rules !sg [rule]

let declare l id st ty : unit =
  try _declare l id st ty
  with e -> raise_as_env e

let define ?(loc=dloc) id op te ty_opt : unit =
  try _define loc id op te ty_opt
  with e -> raise_as_env e

let add_rules (rules: untyped_rule list) : (Subst.Subst.t * typed_rule) list =
  try
    let rs2 = List.map (check_rule !sg) rules in
    Signature.add_rules !sg rules;
    rs2
  with e -> raise_as_env e

let infer ?ctx:(ctx=[]) te =
  try
    let ty = infer !sg ctx te in
    ignore(infer !sg ctx ty);
    ty
  with e -> raise_as_env e

let check ?ctx:(ctx=[]) te ty =
  try check !sg ctx te ty
  with e -> raise_as_env e

let _unsafe_reduction red te =
  Reduction.reduction red !sg te

let _reduction ctx red te =
  ignore(Typing.infer !sg ctx te);
  _unsafe_reduction red te

let reduction ?ctx:(ctx=[]) ?red:(red=Reduction.default_cfg) te =
  try  _reduction ctx red te 
  with e -> raise_as_env e

let unsafe_reduction ?red:(red=Reduction.default_cfg) te =
  try _unsafe_reduction red te
  with e -> raise_as_env e

let are_convertible ?ctx:(ctx=[]) te1 te2 =
  try
    ignore(Typing.infer !sg ctx te1);
    ignore(Typing.infer !sg ctx te2);
    Reduction.are_convertible !sg te1 te2
  with e -> raise_as_env e
