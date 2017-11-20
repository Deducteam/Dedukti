open Basic
open Term
open Rule
open Typing
open Signature

type env_error =
  | EnvErrorType of typing_error
  | EnvErrorSignature of signature_error
  | KindLevelDefinition of loc*ident

(* Wrapper around Signature *)

let sg = ref (Signature.make (mk_mident "noname"))

let init name = sg := Signature.make name

let get_name () = Signature.get_name !sg

let get_type l cst =
  try OK (Signature.get_type !sg l cst)
  with SignatureError e -> Err e

let get_dtree l cst =
  try OK (Signature.get_dtree !sg l cst)
  with SignatureError e -> Err e

let export () : bool = Signature.export !sg

let _declare (l:loc) (id:ident) st ty : unit =
  match inference !sg ty with
    | Kind | Type _ -> Signature.add_declaration !sg l id st ty
    | s -> raise (TypingError (SortExpected (ty,[],s)))

exception DefineExn of loc*ident

let _define (l:loc) (id:ident) (te:term) (ty_opt:typ option) : unit =
  let ty = match ty_opt with
    | None -> inference !sg te
    | Some ty -> ( checking !sg te ty; ty )
  in
  match ty with
  | Kind -> raise (DefineExn (l,id))
  | _ ->
    _declare l id Signature.Definable ty;
    let cst = mk_name (get_name ()) id in
    let name = Delta(cst) in
    let rule =
      { name ;
        ctx = [] ;
        pat = Pattern(l, cst, []);
        rhs = te ;
      }
    in
    Signature.add_rules !sg [rule]

let _define_op (l:loc) (id:ident) (te:term) (ty_opt:typ option) : unit =
  let ty = match ty_opt with
    | None -> inference !sg te
    | Some ty -> ( checking !sg te ty; ty )
  in
  match ty with
  | Kind -> raise (DefineExn (l,id))
  | _ -> Signature.add_declaration !sg l id Signature.Static ty

let declare l id st ty : (unit,env_error) error =
  try OK ( _declare l id st ty )
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let define l id te ty_opt : (unit,env_error) error =
  try OK ( _define l id te ty_opt )
  with
  | SignatureError e -> Err (EnvErrorSignature e)
  | TypingError e -> Err (EnvErrorType e)
  | DefineExn (l,id) -> Err (KindLevelDefinition (l,id))

let define_op l id te ty_opt =
  try OK ( _define_op l id te ty_opt )
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)
    | DefineExn (l,id) -> Err (KindLevelDefinition (l,id))

let add_rules (rules: untyped_rule list) : (typed_rule list,env_error) error =
  try
    let rs2 = List.map (check_rule !sg) rules in
    Signature.add_rules !sg rs2;
    OK rs2
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let infer ?ctx:(ctx=[]) ?red:(red=Reduction.default) strategy te =
  try
    let ty = Typing.infer !sg ctx te in
    let _  = inference !sg ty in
    Reduction.select red;
    let ty' = (Reduction.reduction !sg strategy ty) in
    Reduction.select Reduction.default;
    OK (ty')
  with
  | SignatureError e -> Err (EnvErrorSignature e)
  | TypingError e -> Err (EnvErrorType e)

let check ?ctx:(ctx=[]) te ty =
  try OK (ignore(check !sg ctx te ty))
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let reduction ?red:(red=Reduction.default) strategy te =
  try
    let _ = inference !sg te in
    Reduction.select red;
    let te' = (Reduction.reduction !sg strategy te) in
    Reduction.select Reduction.default;
    OK te'
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let unsafe_one_step ?red:(red=Reduction.default) te =
  Reduction.select red;
  let te' = Reduction.reduction !sg (Reduction.NSteps 1) te in
  Reduction.select Reduction.default;
  te'

let unsafe_snf ?red:(red=Reduction.default) te =
  Reduction.select red;
  let te' = Reduction.reduction !sg Reduction.Snf te in
  Reduction.select Reduction.default;
  te'

let are_convertible ?red:(red=Reduction.default) te1 te2 =
  try
    let _ = inference !sg te1 in
    let _ = inference !sg te2 in
    Reduction.select red;
    let b = Reduction.are_convertible !sg te1 te2 in
    Reduction.select Reduction.default;
    OK b
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)
