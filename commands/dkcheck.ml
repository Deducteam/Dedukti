open Term
open Basic
open Parser
open Entry

let eprint lc fmt =
  Debug.(debug d_notice ("%a " ^^ fmt) pp_loc lc)

let mk_entry md e =
  match e with
  | Decl(lc,id,st,ty) ->
    eprint lc "Declaration of constant '%a'." pp_ident id;
    Errors.fail_if_err (Env.declare lc id st ty)
  | Def(loc,id,opaque,ty,te) ->
    let opaque_str = if opaque then " (opaque)" else "" in
    eprint loc "Definition of symbol '%a'%s." pp_ident id opaque_str;
    Errors.fail_if_err (Env.define ~loc:loc id opaque te ty)
  | Rules(rs) ->
    let open Rule in
    let get_infos p =
      match p with
      | Pattern(l,cst,_) -> (l,cst)
      | _                -> (dloc,mk_name (mk_mident "") dmark)
    in
    let r = List.hd rs in (* cannot fail. *)
    let (l,cst) = get_infos r.pat in
    eprint l "Adding rewrite rules for '%a'" pp_name cst;
    let rs = Errors.fail_if_err (Env.add_rules rs) in
    List.iter (fun (s,r) ->
        eprint (get_loc_pat r.pat) "%a@.with the following constraints: %a"
          pp_typed_rule r Subst.Subst.pp s) rs
  | Eval(_,red,te) ->
    let te = Errors.fail_if_err (Env.reduction ~red te) in
    Format.printf "%a@." Pp.print_term te
  | Infer(_,red,te) ->
    let  ty = Errors.fail_if_err (Env.infer te) in
    let rty = Errors.fail_if_err (Env.reduction ~red ty) in
    Format.printf "%a@." Pp.print_term rty
  | Check(l, assrt, neg, Convert(t1,t2)) ->
    begin
      match Env.are_convertible t1 t2 with
      | OK ok when ok = not neg -> if not assrt then Format.printf "YES@."
      | OK _  when assrt        -> failwith (Format.sprintf "At line %d: Assertion failed." (fst (of_loc l)))
      | OK _                    -> Format.printf "NO@."
      | Err e                   -> Errors.fail_env_error e
    end
  | Check(l, assrt, neg, HasType(te,ty)) ->
    begin
      match Env.check te ty with
      | OK () when not neg -> if not assrt then Format.printf "YES@."
      | Err _ when neg     -> if not assrt then Format.printf "YES@."
      | OK () when assrt   -> failwith (Format.sprintf "At line %d: Assertion failed." (fst (of_loc l)))
      | Err _ when assrt   -> failwith (Format.sprintf "At line %d: Assertion failed." (fst (of_loc l)))
      | _                  -> Format.printf "NO@."
    end
  | DTree(lc,m,v) ->
    let m = match m with None -> Env.get_name () | Some m -> m in
    let cst = mk_name m v in
    let forest = Errors.fail_if_err (Env.get_dtree lc cst) in
    Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest forest
  | Print(_,s) -> Format.printf "%s@." s
  | Name(_,n) ->
    if not (mident_eq n md)
    then Debug.(debug d_warn "Invalid #NAME directive ignored.@.")
  | Require(lc,md) -> Errors.fail_if_err (Env.import lc md)

let mk_entry beautify md =
  if beautify then Pp.print_entry Format.std_formatter
  else mk_entry md


let run_on_file beautify export file =
  let input = open_in file in
  Debug.(debug d_module "Processing file '%s'..." file);
  let md = Env.init file in
  Confluence.initialize ();
  Parser.handle_channel md (mk_entry beautify md) input;
  if not beautify then
    Errors.success "File '%s' was successfully checked." file;
  Errors.fail_if_err (Env.export ());
  Confluence.finalize ();
  close_in input


let _ =
  let run_on_stdin = ref None  in
  let export       = ref false in
  let beautify     = ref false in
  let options = Arg.align
    [ ( "-d"
      , Arg.String Debug.set_debug_mode
      , " flags enables debugging for all given flags [qnocutrm]" )
    ; ( "-q"
      , Arg.Unit (fun () -> Debug.set_debug_mode "q")
      , " Quiet mode (equivalent to -d 'q'" )
    ; ( "-e"
      , Arg.Set export
      , " Generates an object file (\".dko\")" )
    ; ( "-nc"
      , Arg.Clear Errors.color
      , " Disable colors in the output" )
    ; ( "-stdin"
      , Arg.String (fun n -> run_on_stdin := Some(n))
      , " MOD Parses standard input using module name MOD" )
    ; ( "-version"
      , Arg.Unit (fun () -> Format.printf "Dedukti %s@." Version.version)
      , " Print the version number" )
    ; ( "-coc"
      , Arg.Set Typing.coc
      , " Typecheck the Calculus of Construction" )
    ; ( "-I"
      , Arg.String Basic.add_path
      , " DIR Add the directory DIR to the load path" )
    ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
      , " Normalize the types in error messages" )
    ; ( "-cc"
      , Arg.String Confluence.set_cmd
      , " CMD Set the external confluence checker command to CMD" )
    ; ( "-nl"
      , Arg.Set Rule.allow_non_linear
      , " Allow non left-linear rewriting rules" )
    ; ( "--beautify"
      , Arg.Set beautify
      , " Pretty printer. Print on the standard output" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  if !beautify && !export then
    begin
      Format.eprintf "Beautify and export cannot be set at the same time@.";
      exit 2
    end;
  try
    List.iter (run_on_file !beautify !export) files;
    match !run_on_stdin with
    | None   -> ()
    | Some m ->
      let md = Env.init m in
      Parser.handle_channel md (mk_entry !beautify md) stdin;
      if not !beautify
      then Errors.success "Standard input was successfully checked.\n"
  with
  | Parse_error(loc,msg) -> Format.eprintf "Parse error at (%a): %s@." pp_loc loc msg; exit 1
  | Sys_error err        -> Format.eprintf "ERROR %s.@." err; exit 1
  | Exit                 -> exit 3
