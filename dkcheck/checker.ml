open Basic

(* ********************************* *)

let verbose = ref false

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c;
    Format.kfprintf (fun _ -> prerr_newline () ) Format.err_formatter fmt
  ) else
    Format.ifprintf Format.err_formatter fmt

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_ident name;
  Env.init name;
  Confluence.initialize ()

let mk_declaration lc id pty : unit =
  eprint lc "Declaration of constant '%a'." pp_ident id;
  match Env.declare_constant lc id pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_definable lc id pty : unit =
  eprint lc "Declaration of definable '%a'." pp_ident id;
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

let get_infos = function
  | Rule.Pattern (l,md,id,_) -> (l,md,id)
  | _ -> (dloc,qmark,qmark)

let mk_rules = function
  | [] -> ()
  | (rule::_) as lst ->
    begin
      let (_,pat,_) = rule.Rule.rule in
      let (l,md,id) = get_infos pat in
      eprint l "Adding rewrite rules for '%a.%a'" pp_ident md pp_ident id;
      match Env.add_rules lst with
      | OK lst2 ->
        List.iter ( fun rule ->
            let (_,pat,_) = rule.Rule.rule in
            eprint (Rule.get_loc_pat pat) "%a" Rule.pp_typed_rule rule
          ) lst2 ;
      | Err e -> Errors.fail_env_error e
    end

let mk_command = Cmd.mk_command

let export = ref false

let mk_ending () =
  ( if !export then
    if not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ()) );
  Confluence.finalize ()
