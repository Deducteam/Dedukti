open Basic

(* ********************************* *)

let verbose = ref false
let sizechange = ref false
let szgraph = ref false
                         
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

let mk_declaration lc id st pty : unit =
  eprint lc "Declaration of constant '%a'." pp_ident id;
  if (!sizechange)|| (!szgraph) then Sizechange.add_fonc !verbose id pty;
  match Env.declare lc id st pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  if (!sizechange)|| (!szgraph) then Sizechange.add_symb !verbose id pte;
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

let mk_rules = Rule.( function
  | [] -> ()
  | (rule::_) as lst ->
    begin
      let (l,md,id) = get_infos rule.pat in
      eprint l "Adding rewrite rules for '%a.%a'" pp_ident md pp_ident id;
      if (!sizechange)||(!szgraph) then Sizechange.add_rules !verbose lst;
      match Env.add_rules lst with
      | OK lst2 ->
        List.iter ( fun rule ->
            eprint (get_loc_pat rule.pat) "%a" pp_typed_rule rule
          ) lst2 ;
      | Err e -> Errors.fail_env_error e
    end
  )
let mk_command = Cmd.mk_command

let export = ref false

let mk_ending () =
  if (!sizechange)|| (!szgraph) then if Sizechange.sct_only () then Format.printf "La réécriture termine d'après le SCP\n" else Format.printf "La réécriture ne termine PAS d'après les SCP\n";
  ( if !export then
    if not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ()) );
  Confluence.finalize ();

  if !szgraph then Sizechange.latex_print_calls()
