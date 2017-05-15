open Basic
open Term

let mk_prelude (lc:loc) (id:ident) =
  Env.init id

let mk_declaration (lc:loc) (id:ident) (te:term) =
  match Env.declare_constant lc id te with
  | OK () ->
    begin
      match Hol.compile_declaration lc id te with
      | OK(obj) -> ()
      | Err(err) -> failwith "todo"
    end
  | Err er -> Errors.fail_env_error er

let mk_definable (lc:loc) (id:ident) (te:term) =
  match Env.declare_definable lc id te with
  | OK () -> ()
  | Err er -> Errors.fail_env_error er

let mk_definition (lc:loc) (id:ident) (pty:term option) (te:term) =
  match Env.define lc id te pty with
  | OK () -> ()
  | Err er -> Errors.fail_env_error er

let mk_opaque (lc:loc) (id:ident) (pty:term option) (te:term) =
  match Env.define_op lc id te pty with
  | OK () -> ()
  | Err er -> Errors.fail_env_error er

let mk_rules (rules:Rule.rule list) =
  match Env.add_rules rules with
  | OK _ -> ()
  | Err er -> Errors.fail_env_error er

let mk_command (lc:loc) (cmd:Cmd.command) =
  Cmd.mk_command lc cmd

let mk_ending () =
  if not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ());
