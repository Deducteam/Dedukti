open Basics
open Term
open Rule

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

let mk_prelude _ _ = failwith "Top.prelude should not be used."

let mk_declaration lc id pty =
  Typing.declare lc id pty;
  print "%s is declared." (string_of_ident id)

let mk_definition lc id pty_opt pte =
  Typing.define lc id pte pty_opt;
  print "%s is defined." (string_of_ident id)

let mk_opaque lc id pty_opt pte =
  Typing.define_opaque lc id pte pty_opt;
  print "%s is declared." (string_of_ident id)

let mk_rules rs0 =
  let rs = List.map Underscore.refine_rule rs0 in
    Typing.add_rules rs;
    List.iter (fun r -> print "%a" Pp.pp_rule r) rs

let mk_command = Cmd.mk_command

let mk_ending _ = ()
