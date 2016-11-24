open Basics

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

let mk_prelude _ _ = failwith "Top.mk_prelude"

let mk_declaration lc id pty =
  match Env.declare_constant lc id pty with
    | OK _ -> print "%a is declared." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_definable lc id pty =
  match Env.declare_definable lc id pty with
    | OK _ -> print "%a is declared." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte =
  match Env.define lc id pte pty_opt with
    | OK _ -> print "%a is defined." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  match Env.define_op lc id pte pty_opt with
    | OK _ -> print "%a is declared." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_rules lst =
  match Env.add_rules lst with
    | OK _ -> List.iter (fun r -> print "%a" Rule.pp_rule r) lst
    | Err e -> Errors.fail_env_error e

let mk_command = Cmd.mk_command

let mk_ending _ = ()
