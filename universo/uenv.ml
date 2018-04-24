open Basic
open Term
open Rule
open Typing
open Signature

type cfg =
  {
    elaborating:bool;
    checking:bool;
    solving:bool;
    log:int;
  }

let env = ref {elaborating=true;checking=true;solving=true;log=0}

let sg = ref (Signature.make "noname")

let log i fmt =
  let open Format in
  if !env.log >= i
    then kfprintf (fun _ -> pp_print_newline err_formatter ()) err_formatter fmt
    else ifprintf err_formatter fmt

let solve () =
  let cs = Constraints.export () in
  let i, model = Export.Z3.solve cs in
  (i, model)

let get_signature () = !sg

let mk_cfg cfg = env := cfg

let init file =
  sg := Signature.make file ;
  Signature.get_name !sg

let get_name () = Signature.get_name !sg

let add_name name = sg := Signature.add_name !sg name

let get_type l cst =
  try OK (Signature.get_type !sg l cst) with SignatureError e -> Err e

let get_dtree l cst =
  try OK (Signature.get_dtree !sg None l cst) with SignatureError e -> Err e

let export () : bool = Signature.export !sg

let import lc md =
  try OK (Signature.import !sg lc md) with SignatureError e -> Err e

let elaborate ctx is_prop te = snd @@ Elaboration.elaborate !sg ctx is_prop te

let elaborate_type ty =
  let s,_,x = Elaboration.elaborate_term !sg [] ty in s,x

let _declare (l: loc) (id: ident) st ty : unit =
  log 1 "Declare %a@." Pp.print_ident id;
  let _,ty' = elaborate_type ty in
  log 2 "%a@." Pp.print_term ty';
  match inference !sg ty' with
  | Kind | Type _ -> Signature.add_declaration !sg l id st ty'
  | s -> raise (TypingError (SortExpected (ty', [], s)))

exception DefineExn of loc * ident

let _define (l: loc) (id: ident) (te: term) (ty_opt: typ option) : unit =
  log 1 "Define %a@." Pp.print_ident id;
  let te', ty_opt' =
    match ty_opt with
    | None ->
      let te' = elaborate [] false te in
      log 2 "%a@." Pp.print_term te;
      te' , None
    | Some ty ->
      log 2 "%a@." Pp.print_term te;
      let s,ty' = elaborate_type ty in
      let te' = elaborate [] (Elaboration.if_prop s) te in
      log 2 "%a@." Pp.print_term ty;
      log 2 "%a@." Pp.print_term te;
      te' , Some ty'
  in
  let ty' =
    match ty_opt' with
    | None -> inference !sg te'
    | Some ty ->
        checking !sg te' ty ;
        ty
  in
  match ty' with
  | Kind -> raise (DefineExn (l, id))
  | _ ->
      _declare l id Signature.Definable ty' ;
      let cst = mk_name (get_name ()) id in
      let rule =
        {name= Delta cst; ctx= []; pat= Pattern (l, cst, []); rhs= te'}
      in
      Signature.add_rules !sg [rule]

let _define_op (l: loc) (id: ident) (te: term) (ty_opt: typ option) : unit =
  log 1 "Define Opaque %a@." Pp.print_ident id;
  let te', ty_opt' =
    match ty_opt with
    | None ->
      let te' = elaborate [] false te in
      log 2 "%a@." Pp.print_term te;
      te' , None
    | Some ty ->
      log 2 "%a@." Pp.print_term te;
      let s,ty' = elaborate_type ty in
      let te' = elaborate [] (Elaboration.if_prop s) te in
      log 2 "%a@." Pp.print_term ty;
      log 2 "%a@." Pp.print_term te;
      te' , Some ty'
  in
  let ty' =
    match ty_opt' with
    | None -> inference !sg te'
    | Some ty ->
        checking !sg te' ty ;
        ty
  in
  match ty' with
  | Kind -> raise (DefineExn (l, id))
  | _ -> Signature.add_declaration !sg l id Signature.Static ty'

let declare l id st ty : (unit, Env.env_error) error =
  try OK (_declare l id st ty) with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError e -> Err (Env.EnvErrorType e)

let define l id te ty_opt : (unit, Env.env_error) error =
  try OK (_define l id te ty_opt) with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError e -> Err (Env.EnvErrorType e)
  | DefineExn (l, id) -> Err (Env.KindLevelDefinition (l, id))

let define_op l id te ty_opt =
  try OK (_define_op l id te ty_opt) with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError e -> Err (Env.EnvErrorType e)
  | DefineExn (l, id) -> Err (Env.KindLevelDefinition (l, id))

let add_rules (rules: untyped_rule list)
    : (typed_rule list, Env.env_error) error =
  try
    Reduction.just_check := true;
    let rs2 = List.map (check_rule !sg) rules in
    Reduction.just_check := false;
    let ctx_of_rctx ctx =
      let open Elaboration in
      List.fold_left (fun ctx (l,x,t) ->
          let s',u',_ = elaborate_term !sg ctx t in
          ((x,{ty=u'; sort=s'})::ctx)) [] ctx
    in
    let forget_types ctx = List.map (fun (lc,id,_) -> (lc,id)) ctx in
    let elab_rules =
      List.map (fun r -> {r with rhs = elaborate (ctx_of_rctx r.ctx) false r.rhs;
                                 ctx = forget_types r.ctx}) rs2 in
    let rs2 = List.map (check_rule !sg) elab_rules in
    Signature.add_rules !sg rules ;
    OK rs2
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError e -> Err (Env.EnvErrorType e)

let infer ?(ctx= []) te =
  try
    let ty = infer !sg ctx te in
    ignore (infer !sg ctx ty) ;
    OK ty
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError e -> Err (Env.EnvErrorType e)

let check ?(ctx= []) te ty =
  try OK (ignore (check !sg ctx te ty)) with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError e -> Err (Env.EnvErrorType e)

let reduction ?(ctx= []) ?(red= Reduction.default_cfg) te =
  try
    ignore (Typing.infer !sg ctx te) ;
    let te' = Reduction.reduction red !sg te in
    OK te'
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError e -> Err (Env.EnvErrorType e)

let unsafe_reduction ?(red= Reduction.default_cfg) te =
  let te' = Reduction.reduction red !sg te in
  te'

let are_convertible ?(ctx= []) te1 te2 =
  try
    ignore (Typing.infer !sg ctx te1) ;
    ignore (Typing.infer !sg ctx te2) ;
    let b = Reduction.are_convertible !sg te1 te2 in
    OK b
  with
  | SignatureError e -> Err (Env.EnvErrorSignature e)
  | TypingError e -> Err (Env.EnvErrorType e)
