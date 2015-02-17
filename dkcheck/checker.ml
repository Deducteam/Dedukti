open Basics

(* ********************************* *)

let verbose = ref false

let set_debug_level lvl =
  if lvl > 0 then ( verbose := true; Pp.print_db_enabled := true )

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c;
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt
  ) else
    Printf.ifprintf stderr fmt

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_ident name;
  Env.init name

let mk_declaration lc id pty : unit =
  eprint lc "Declaration of symbol '%a'." pp_ident id;
  match Env.declare_definable lc id pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  match Env.define lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_rules lst =
  List.iter (
    fun (ctx,pat,rhs) ->
      eprint (Rule.get_loc_pat pat) "%a" Rule.pp_rule (ctx,pat,rhs)
  ) lst ;
  match Env.add_rules lst with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_command = Cmd.mk_command

let export = ref false

let mk_ending () =
  if !export then
    if not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ())
