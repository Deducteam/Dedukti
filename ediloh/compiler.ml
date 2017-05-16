open Basic
open Term

let mk_prelude (lc:loc) (id:ident) =
  Env.init id

let mk_declaration (lc:loc) (id:ident) (st:Signature.staticity) (te:term) =
  match st with
  | Signature.Static ->
    begin
      match Env.declare lc id st te with
      | OK () ->
        begin
          match Hol.compile_declaration lc id te with
          | OK(obj) -> Hol.compile_hol_obj obj
          | Err(err) -> Hol.fail_compile_declaration err
        end
      | Err er -> Errors.fail_env_error er
    end
  | Signature.Definable ->
    Errors.fail lc "Definable declaration are not handle by the compiler because no rule should be associated with it. Maybe the declaration should be static."
  | Signature.Injective ->
    Errors.fail lc "Injective declaration are not handle by the compiler, but maybe it could..."

let mk_definition (lc:loc) (id:ident) (pty:term option) (te:term) =
  match Env.define lc id te pty with
  | OK () ->
    begin
      match pty with
      | None ->
        Errors.fail lc "Every definition should be typed"
      | Some ty ->
        begin
          match Hol.compile_definition lc id ty te with
          | OK(obj) -> Hol.compile_hol_obj obj
          | Err(err) -> Hol.fail_compile_definition err
        end
    end
  | Err er -> Errors.fail_env_error er

let mk_opaque (lc:loc) (id:ident) (pty:term option) (te:term) =
  match Env.define_op lc id te pty with
  | OK () -> ()
  | Err er -> Errors.fail_env_error er

let mk_rules (rules:Rule.untyped_rule list) =
  match Env.add_rules rules with
  | OK _ -> ()
  | Err er -> Errors.fail_env_error er

let mk_command (lc:loc) (cmd:Cmd.command) =
  Cmd.mk_command lc cmd

let mk_ending () =
  if not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ());
