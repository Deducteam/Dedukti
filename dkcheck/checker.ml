open Basic
open Cmd

(* ********************************* *)

let verbose = ref false
let sizechange = ref false
let szgraph = ref false
let szvb = ref false
                  
let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c;
    Format.kfprintf (fun _ -> prerr_newline () ) Format.err_formatter fmt
  ) else
    Format.ifprintf Format.err_formatter fmt

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_mident name;
  Env.init name;
  Confluence.initialize ()

let mk_declaration lc id st pty : unit =
  eprint lc "Declaration of constant '%a'." pp_ident id;
  match Env.declare lc id st pty with
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
  | Rule.Pattern (l,cst,_) -> (l,cst)
  | _ -> (dloc,mk_name (mk_mident "") qmark)

let mk_rules = Rule.( function
  | [] -> ()
  | (rule::_) as lst ->
    begin
      let (l,cst) = get_infos rule.pat in
      eprint l "Adding rewrite rules for '%a'" pp_name cst;
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
    let cst = mk_name m v in
        ( match Env.get_dtree lc cst with
            | OK (Some (i,g)) ->
                Format.printf "%a\n" Dtree.pp_rw (cst,i,g)
            | _ -> Format.printf "No GDT.@." )
  | Print str         -> Format.printf "%s@." str
  | Require m         ->
    ( match Env.import lc m with
      | OK () -> ()
      | Err e -> Errors.fail_signature_error e )
  | Other (cmd,_)     -> Format.eprintf "Unknown command '%s'.@." cmd

let export = ref false

let mk_ending () =
  let red_error fmt= Format.eprintf "\027[31mERROR \027[m";
    Format.kfprintf (fun _ -> Format.pp_print_newline Format.err_formatter () ) Format.err_formatter fmt
  in
  if (!sizechange|| !szgraph|| !szvb) then
    begin
      try (
	if Env.sizechange (!verbose|| !szvb) !szgraph
	then Errors.success "The file was proved terminating"
	else red_error "The SCT checker did not proved this file terminating"
      )
      with
      | Sizechange.Ext_ru e-> red_error "The meaning of external_rules is actually unknown, hence the case where it is not empty is unaccepted yet. Here this variable contains %a" (pp_list " ; " (pp_list "," Rule.pp_rule_infos)) e
      | Sizechange.Calling_unknown i -> red_error "The caller line %i is unknown" i
      | Sizechange.NonLinearity i -> red_error "The rule declared line %i is not linear" i
      | Sizechange.PatternMatching i -> red_error "The rule declared line %i require to pattern match on defined symbol" i
      | Sizechange.TypingError (m,f) -> red_error "There is a typing error with the symbol %a.%a" pp_ident m pp_ident f
      | Sizechange.NonPositivity (m,f) -> red_error "The symbol %a.%a is not strictly positive" pp_ident m pp_ident f
      | Sizechange.TarjanError -> red_error "Problem with the Tarjan algorithm"
      | Sizechange.Callee_unknown (m,v,i) -> red_error "The callee %a.%a line %i is unknown" pp_ident m pp_ident v i
      | Sizechange.TypeLevelRewriteRule (a,b,c,d) -> red_error "Type level rewriting between %a.%a and %a.%a" pp_ident a pp_ident b pp_ident c pp_ident d
      | Sizechange.TypeLevelWeird (a,b,t) -> red_error "Type level contains something weird in definition of %a.%a : %a" pp_ident a pp_ident b Term.pp_term t
      | Sizechange.ModuleDependancy (a,b) -> red_error "Module dependancy %a.%a" pp_ident a pp_ident b
    end;
  if !export then
    if not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a'." pp_mident (Env.get_name ()) );
  Confluence.finalize ()
