open Basics
open Rule
open Env

(* ********************************* *)

let verbose = ref false

let set_debug_level lvl =
  if lvl > 0 then ( verbose := true; Pp.print_db := true )

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c;
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt
  ) else
    Printf.ifprintf stderr fmt

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_ident name;
  Env.init name

let mk_declaration lc id pty =
  eprint lc "Declaration of symbol '%a'." pp_ident id;
  SafeEnv.add_decl lc id pty

let mk_definition lc id pty_opt pte =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  SafeEnv.add_def lc id pte pty_opt

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  SafeEnv.add_opaque lc id pte pty_opt

let mk_rules : rule list -> unit = function
  | [] -> ()
  | (r::_) as lst -> ( eprint r.l "Rewrite rule for symbol '%a'." pp_ident r.id ;
                       SafeEnv.add_rules lst )

let mk_command = Cmd.mk_command

let export = ref false

let mk_ending _ = if !export then Env.export ()
