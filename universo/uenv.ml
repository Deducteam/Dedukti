open Basic
open Term
open Rule
open Typing
open Signature

let current_level = ref 1

let before_cs = ref Constraints.ConstraintsSet.empty
let check_bump id  = false (*
  let cs = Constraints.export () in
  let i,_ = Export.Z3.solve (Constraints.optimize cs) in
  current_level := i;
  Format.eprintf "%a@." pp_ident id;
  if !current_level > 1 then
    begin
      Format.eprintf "bump:%a@." pp_ident id;
      Constraints.import !before_cs;
      current_level := 1;
      true
    end
  else
    begin
      before_cs := cs;
      false
    end
*)
let solve () =
  let cs = Constraints.export () in
  let i,model = Export.Z3.solve cs in
  i,model

let sg = ref (Signature.make "noname")

let get_signature () = !sg

let init file =
  sg := Signature.make file;
  Signature.get_name !sg

let get_name () = Signature.get_name !sg

let add_name name = sg := Signature.add_name !sg name

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
  try
    match inference !sg ty with
    | Kind | Type _ ->
      if check_bump id then
        ()
      else
        Signature.add_declaration !sg l id st ty
    | s -> raise (TypingError (SortExpected (ty,[],s)))
  with Signature.SignatureError(SymbolNotFound _) -> Format.eprintf "dep:%a@." pp_ident id

exception DefineExn of loc*ident

let _define (l:loc) (id:ident) (te:term) (ty_opt:typ option) : unit =
  try
    let ty = match ty_opt with
      | None -> inference !sg te
      | Some ty -> ( checking !sg te ty; ty )
    in
    match ty with
    | Kind -> raise (DefineExn (l,id))
    | _ ->
      if check_bump id then
        ()
      else
        begin
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
        end
  with Signature.SignatureError(SymbolNotFound _) -> Format.eprintf "dep:%a@." pp_ident id

let _define_op (l:loc) (id:ident) (te:term) (ty_opt:typ option) : unit =
  try
    let ty = match ty_opt with
      | None -> inference !sg te
      | Some ty -> ( checking !sg te ty; ty )
    in
    match ty with
    | Kind -> raise (DefineExn (l,id))
    | _ ->
      if check_bump id then
        ()
      else
        Signature.add_declaration !sg l id Signature.Static ty
  with Signature.SignatureError(SymbolNotFound _) -> Format.eprintf "dep:%a@." pp_ident id

let declare l id st ty : (unit,Env.env_error) error =
  try OK ( _declare l id st ty )
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError    e -> Err (Env.EnvErrorType e)

let define l id te ty_opt : (unit,Env.env_error) error =
  try OK ( _define l id te ty_opt )
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError    e -> Err (Env.EnvErrorType e)
  | DefineExn (l,id) -> Err (Env.KindLevelDefinition (l,id))

let define_op l id te ty_opt =
  try OK ( _define_op l id te ty_opt )
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError    e -> Err (Env.EnvErrorType e)
  | DefineExn (l,id) -> Err (Env.KindLevelDefinition (l,id))

let add_rules (rules: untyped_rule list) : (typed_rule list,Env.env_error) error =
  try
    let rs2 = List.map (check_rule !sg) rules in
    try
      if check_bump (mk_ident "some rule") then
        OK rs2
      else
        begin
          Signature.add_rules !sg rs2;
          OK rs2
        end
    with _ -> Format.eprintf "some rule@."; OK rs2
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError    e -> Err (Env.EnvErrorType e)


let infer ?ctx:(ctx=[]) te =
  try
    let ty = infer !sg ctx te in
    ignore(infer !sg ctx ty);
    OK ty
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError    e -> Err (Env.EnvErrorType e)

let check ?ctx:(ctx=[]) te ty =
  try OK (ignore(check !sg ctx te ty))
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError    e -> Err (Env.EnvErrorType e)

let reduction ?ctx:(ctx=[]) ?red:(red=Reduction.default_cfg) te =
  try
    ignore(Typing.infer !sg ctx te);
    let te' = Reduction.reduction red !sg te in
    OK te'
  with
    | SignatureError e -> Err (Env.EnvErrorSignature e)
    | TypingError    e -> Err (Env.EnvErrorType e)

let unsafe_reduction ?red:(red=Reduction.default_cfg) te =
  let te' = Reduction.reduction red !sg te in
  te'

let are_convertible ?ctx:(ctx=[]) te1 te2 =
  try
    ignore(Typing.infer !sg ctx te1);
    ignore(Typing.infer !sg ctx te2);
    let b = Reduction.are_convertible !sg te1 te2 in
    OK b
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError    e -> Err (Env.EnvErrorType e)
