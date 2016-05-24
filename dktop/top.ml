open Basics

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

type 'a m = 'a
  
type entry = unit
  
let return a = a
  
let bind t f = f t 
  
let mk_Type loc = return (Term.mk_Type loc)
  
let mk_DB loc id n = return (Term.mk_DB loc id n)
  
let mk_Const loc md id = return (Term.mk_Const loc md id)
  
let mk_Lam loc id ty t = return (Term.mk_Lam loc id ty t)
  
let mk_App f t ts = return (Term.mk_App f t ts)

let mk_Pi loc x ta tb = return (Term.mk_Pi loc x ta tb)

let mk_Arrow loc ta tb = return (Term.mk_Arrow loc ta tb)
  
let mk_ending _ = ()

let mk_prelude _ _ = failwith "Top.mk_prelude"

let mk_declaration lc id pty =
  match Env.declare_constant lc id pty with
    | OK () -> print "%a is declared." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_definable lc id pty =
  match Env.declare_definable lc id pty with
    | OK () -> print "%a is declared." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte =
  match Env.define lc id pte pty_opt with
    | OK () -> print "%a is defined." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  match Env.define_op lc id pte pty_opt with
    | OK () -> print "%a is declared." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_rules lst =
  match Env.add_rules lst with
    | OK _ -> List.iter (fun r -> print "%a" Rule.pp_rule r) lst
    | Err e -> Errors.fail_env_error e

let mk_command l c = failwith "TODO" (* Cmd.mk_command *)

let mk_ending _ = ()
