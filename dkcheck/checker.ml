open Basic

(* ********************************* *)

let verbose = ref false
let sizechange = ref false
let szgraph = ref false
let variable_call = ref (-1)
                  
let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c;
    Format.kfprintf (fun _ -> prerr_newline () ) Format.err_formatter fmt
  ) else
    Format.ifprintf Format.err_formatter fmt

(* ********************************* *)

let mk_prelude lc name =
  if (!sizechange)|| (!szgraph) then (Sizechange.initialize (); variable_call:=(-1); Format.eprintf"\n");
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
  if (!sizechange)|| (!szgraph) then
    (try Sizechange.add_symb !verbose id pte
    with Failure _-> let (l,c) = of_loc lc in variable_call := l);
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
      if (!sizechange)||(!szgraph) then
        (try Sizechange.add_rules !verbose lst
        with Failure _-> let (l,c) = of_loc l in variable_call:=l);
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
  let red_error fmt= Format.eprintf "\027[31mERROR \027[m";
                     Format.kfprintf (fun _ -> Format.pp_print_newline Format.err_formatter () ) Format.err_formatter fmt
  in
  if (!sizechange)|| (!szgraph) then
    if Sizechange.sct_only ()
    then if !variable_call=(-1)
         then Errors.success "Rewriting ends according to the SCP"
         else red_error "SCP does not accept variable in functionnal position, like in line %i" !variable_call
    else (red_error "Rewriting does not end according to the SCP");
  if !export then
    if not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ());
  Confluence.finalize ();
  if !szgraph then Sizechange.latex_print_calls()
