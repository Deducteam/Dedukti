open Basics
open Judgment

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

let mk_prelude _ _ = failwith "Top.prelude should not be used."

let mk_declaration lc id pty =
  declare2 lc id pty;
  print "%a is declared." pp_ident id

let mk_definition lc id pty_opt pte =
  define2 lc id pte pty_opt;
  print "%a is defined." pp_ident id

let mk_opaque lc id pty_opt pte =
  define_op2 lc id pte pty_opt;
  print "%a is declared." pp_ident id

let mk_rules lst =
  add_rules2 lst;
  List.iter (fun r -> print "%a" Pp.pp_rule r) lst

let mk_command = Cmd.mk_command

let mk_ending _ = ()
