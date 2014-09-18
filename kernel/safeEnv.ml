open Term

let add_decl l id pty =
let ty = Inference.is_a_type2 pty in
    Env.add_decl l id ty

let add_opaque l id pte pty_opt =
  let (_,ty) =
    match pty_opt with
      | None          -> Inference.infer2 pte
      | Some pty      -> Inference.check2 pte pty
  in
    Env.add_decl l id ty

let add_def l id pte pty_opt =
let (te,ty) =
    match pty_opt with
      | None          -> Inference.infer2 pte
      | Some pty      -> Inference.check2 pte pty
  in
    Env.add_def l id te ty

let add_rules plst =
  let lst = List.map Inference.check_prule plst in
    Env.add_rw lst
