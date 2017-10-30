open Basic
open Cmd

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

let mk_command lc = function
  | Whnf te          ->
      ( match Env.reduction Reduction.Whnf  te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | Hnf te           ->
      ( match Env.reduction Reduction.Hnf te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | Snf te           ->
      ( match Env.reduction Reduction.Snf te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | OneStep te       ->
      ( match Env.reduction (Reduction.NSteps 1) te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | NSteps (n,te)    ->
      ( match Env.reduction (Reduction.NSteps n) te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | Conv (te1,te2)  ->
        ( match Env.are_convertible te1 te2 with
            | OK true -> Format.printf "YES@."
            | OK false -> Format.printf "NO@."
            | Err e -> Errors.fail_env_error e )
  | Check (te,ty) ->
        ( match Env.check te ty with
            | OK () -> Format.printf "YES@."
            | Err e -> Errors.fail_env_error e )
  | Infer te         ->
      ( match Env.infer (Reduction.NSteps 0) te with
          | OK ty -> Format.printf "%a@." Pp.print_term ty
          | Err e -> Errors.fail_env_error e )
  | InferSnf te         ->
      ( match Env.infer Reduction.Snf te with
          | OK ty -> Format.printf "%a@." Pp.print_term ty
          | Err e -> Errors.fail_env_error e )
  | Gdt (m0,v)         ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
        ( match Env.get_dtree lc m v with
            | OK (Some (i,g)) ->
                Format.printf "%a\n" Dtree.pp_rw (m,v,i,g)
            | _ -> Format.printf "No GDT.@." )
  | Print str         -> Format.printf "%s@." str
  | Other (cmd,_)     -> Format.eprintf "Unknown command '%s'.@." cmd

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
