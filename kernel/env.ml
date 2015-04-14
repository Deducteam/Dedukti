open Basics
open Term
open Rule
open Typing
open Signature

type env_error =
  | EnvErrorType of typing_error
  | EnvErrorSignature of signature_error

(* Wrapper around Signature *)

let sg = ref (Signature.make (hstring "noname"))

let init name = sg := Signature.make name

let get_name () = Signature.get_name !sg

let get_type l md id =
  try OK (Signature.get_type !sg l md id)
  with SignatureError e -> Err e

let get_dtree l md id =
  try OK (Signature.get_dtree !sg l md id)
  with SignatureError e -> Err e

let export () : bool = Signature.export !sg

let _declare (l:loc) (id:ident) (jdg:judgment) : unit =
  assert ( Context.is_empty jdg.ctx );
  match jdg.ty with
    | Kind | Type _ -> Signature.declare !sg l id jdg.te
    | _ -> raise (TypingError (SortExpected (jdg.te,[],jdg.ty)))

let _define (l:loc) (id:ident) (jdg:judgment) =
  assert ( Context.is_empty jdg.ctx );
  Signature.define !sg l id jdg.te jdg.ty

let _define_op (l:loc) (id:ident) (jdg:judgment) =
  assert( Context.is_empty jdg.ctx );
  Signature.declare !sg l id jdg.ty

let declare l id ty : (unit,env_error) error =
  try OK ( _declare l id (inference !sg ty) )
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let define l id te ty_opt =
  try
    match ty_opt with
      | None -> OK ( _define l id (inference !sg te) )
      | Some ty -> OK ( _define l id (checking !sg te ty) )
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let define_op l id te ty_opt =
  try
    match ty_opt with
      | None -> OK ( _define_op l id (inference !sg te) )
      | Some ty -> OK ( _define_op l id (checking !sg te ty) )
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let add_rules (rules: rule list) : (unit,env_error) error =
  try
    let _ = List.iter (check_rule !sg) rules in
      OK (Signature.add_rules !sg rules)
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let infer te =
  try  OK (inference !sg te).ty
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let check te ty =
  try OK (ignore(checking !sg te ty))
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let whnf te =
  try
    let _ = inference !sg te in OK (Reduction.whnf !sg te)
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let hnf te =
  try
    let _ = inference !sg te in OK (Reduction.hnf !sg te)
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let snf te =
  try
    let _ = inference !sg te in OK (Reduction.snf !sg te)
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let unsafe_snf te = Reduction.snf !sg te

let one te =
  try
    let _ = inference !sg te in OK (Reduction.one_step !sg te)
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)

let are_convertible te1 te2 =
  try
    let _ = inference !sg te1 in
    let _ = inference !sg te2 in
      OK (Reduction.are_convertible !sg te1 te2)
  with
    | SignatureError e -> Err (EnvErrorSignature e)
    | TypingError e -> Err (EnvErrorType e)
