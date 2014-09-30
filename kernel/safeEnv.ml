open Term

let add_decl l id ty = Inference.is_a_type [] ty ; Env.declare l id ty

let add_opaque l id te ty_opt =
  let ty = match ty_opt with
    | None    -> Inference.infer [] te
    | Some ty -> ( Inference.is_a_type [] ty ; Inference.check [] te ty ; ty )
  in
    Env.declare l id ty

let add_def l id te ty_opt =
  let ty = match ty_opt with
    | None    -> Inference.infer [] te
    | Some ty -> ( Inference.is_a_type [] ty ; Inference.check [] te ty ; ty )
  in
    Env.define l id te ty

let add_rules lst = List.iter Inference.check_rule lst ; Env.add_rules lst
