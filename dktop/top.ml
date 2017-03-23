open Basic

let print fmt =
  Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt

let mk_prelude _ _ = failwith "Top.mk_prelude"

let mk_declaration lc id st pty =
  match Env.declare lc id st pty with
    | OK () -> Format.printf "%a is declared.@." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte =
  match Env.define lc id pte pty_opt with
    | OK () -> Format.printf "%a is defined.@." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  match Env.define_op lc id pte pty_opt with
    | OK () -> Format.printf "%a is declared.@." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_rules lst =
  let lst' = List.map (fun (r, _) -> r) lst in
  match Env.add_rules lst' with
    | OK _ -> List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) lst'
    | Err e -> Errors.fail_env_error e

let mk_command = Cmd.mk_command

let mk_ending _ = ()
