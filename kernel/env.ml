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

let sg = ref (Signature.make "noname")

let init file =
  sg := Signature.make file;
  Signature.get_name !sg

let get_name () = Signature.get_name !sg

let get_signature () = !sg

let get_type l cst =
  try OK (Signature.get_type !sg l cst)
  with SignatureError e -> Err e

let get_dtree l cst =
  try OK (Signature.get_dtree !sg None l cst)
  with SignatureError e -> Err e

let export () : bool = Signature.export !sg

let import lc md =
  try OK(Signature.import !sg lc md)
  with SignatureError e -> Err e

let _declare (l:loc) (id:ident) st ty : unit =
  match inference !sg ty with
  | Kind | Type _ -> Signature.add_declaration !sg l id st ty
  | s -> raise (TypingError (SortExpected (ty,[],s)))

exception DefineExn of loc*ident

let is_static lc cst = Signature.is_static !sg lc cst

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
    let rule =
      { name= Delta(cst) ;
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
  | TypingError    e -> Err (EnvErrorType e)

let define l id te ty_opt : (unit,env_error) error =
  try OK ( _define l id te ty_opt )
  with
  | SignatureError e -> Err (EnvErrorSignature e)
  | TypingError    e -> Err (EnvErrorType e)
  | DefineExn (l,id) -> Err (KindLevelDefinition (l,id))

let define_op l id te ty_opt =
  try OK ( _define_op l id te ty_opt )
  with
  | SignatureError e -> Err (EnvErrorSignature e)
  | TypingError    e -> Err (EnvErrorType e)
  | DefineExn (l,id) -> Err (KindLevelDefinition (l,id))

let add_rules (rules: untyped_rule list) : (typed_rule list,env_error) error =
  try
    let rs2 = List.map (check_rule !sg) rules in
    Signature.add_rules !sg rules;
    OK rs2
  with
  | SignatureError e -> Err (EnvErrorSignature e)
  | TypingError    e -> Err (EnvErrorType e)

let infer ?ctx:(ctx=[]) te =
  try
    let ty = infer !sg ctx te in
    ignore(infer !sg ctx ty);
    OK ty
  with
  | SignatureError e -> Err (EnvErrorSignature e)
  | TypingError    e -> Err (EnvErrorType e)

let check ?ctx:(ctx=[]) te ty =
  try OK (ignore(check !sg ctx te ty))
  with
  | SignatureError e -> Err (EnvErrorSignature e)
  | TypingError    e -> Err (EnvErrorType e)

let get_reduction_function (red:Reduction.red_cfg) =
  let open Reduction in
  ParamReducer.set_strategy red;
  ParamReducer.reset ();
  match red.strategy with
  | Hnf  -> ParamReducer.hnf
  | Snf  -> ParamReducer.snf
  | Whnf -> ParamReducer.whnf

let reduction ?ctx:(ctx=[]) ?red:(red=Reduction.default_cfg) te =
  try
    ignore(Typing.infer !sg ctx te);
    let te' = get_reduction_function red !sg te in
    OK te'
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError    e -> Err (EnvErrorType e)

let unsafe_reduction ?red:(red=Reduction.default_cfg) te =
  let te' = get_reduction_function red !sg te in
  te'

let are_convertible ?ctx:(ctx=[]) te1 te2 =
  try
    ignore(Typing.infer !sg ctx te1);
    ignore(Typing.infer !sg ctx te2);
    let b = Reduction.StdReducer.are_convertible !sg te1 te2 in
    OK b
  with
  | SignatureError e -> Err (EnvErrorSignature e)
  | TypingError    e -> Err (EnvErrorType e)
