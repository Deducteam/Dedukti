open Term
open Basic
open Parser
open Entry

let eprint lc fmt =
  Debug.(debug D_notice ("%a " ^^ fmt) pp_loc lc)

let mk_entry md e =
  match e with
  | Decl(lc,id,st,ty) ->
    eprint lc "Declaration of constant '%a'." pp_ident id;
    Env.declare lc id st ty
  | Def(lc,id,opaque,ty,te) ->
    let opaque_str = if opaque then " (opaque)" else "" in
    eprint lc "Definition of symbol '%a'%s." pp_ident id opaque_str;
    Env.define lc id opaque te ty
  | Rules(l,rs) ->
    let open Rule in
    List.iter (fun (r:untyped_rule) -> eprint l "Adding rewrite rules: '%a'" Pp.print_rule_name r.name) rs;
    let rs = Env.add_rules rs in
    List.iter (fun (s,r) ->
        eprint (get_loc_pat r.pat) "%a@.with the following constraints: %a"
          pp_typed_rule r (Subst.Subst.pp (fun n -> let _,n,_ = List.nth r.ctx n in n)) s) rs
  | Eval(_,red,te) ->
    let te = Env.reduction ~red te in
    Format.printf "%a@." Pp.print_term te
  | Infer(_,red,te) ->
    let  ty = Env.infer te in
    let rty = Env.reduction ~red ty in
    Format.printf "%a@." Pp.print_term rty
  | Check(l, assrt, neg, Convert(t1,t2)) ->
    let succ = (Env.are_convertible t1 t2) <> neg in
    ( match succ, assrt with
      | true , false -> Format.printf "YES@."
      | true , true  -> ()
      | false, false -> Format.printf "NO@."
      | false, true  -> raise (Env.EnvError (l,Env.AssertError)) )
  | Check(l, assrt, neg, HasType(te,ty)) ->
    let succ = try Env.check te ty; not neg with _ -> neg in
    ( match succ, assrt with
      | true , false -> Format.printf "YES@."
      | true , true  -> ()
      | false, false -> Format.printf "NO@."
      | false, true  -> raise (Env.EnvError (l, Env.AssertError)) )
  | DTree(lc,m,v) ->
    let m = match m with None -> Env.get_name () | Some m -> m in
    let cst = mk_name m v in
    let forest = Env.get_dtree lc cst in
    Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest forest
  | Print(_,s) -> Format.printf "%s@." s
  | Name(_,n) ->
    if not (mident_eq n md)
    then Debug.(debug D_warn "Invalid #NAME directive ignored.@.")
  | Require(lc,md) -> Env.import lc md

let mk_entry beautify md =
  if beautify then Pp.print_entry Format.std_formatter
  else mk_entry md


let run_on_file beautify export file =
  let input = open_in file in
  Debug.(debug Signature.D_module "Processing file '%s'..." file);
  let md = Env.init file in
  Confluence.initialize ();
  Parse_channel.handle md (mk_entry beautify md) input;
  if not beautify then
    Errors.success "File '%s' was successfully checked." file;
  if export then
    Env.export ();
  Confluence.finalize ();
  close_in input


let _ =
  let run_on_stdin = ref None  in
  let export       = ref false in
  let beautify     = ref false in
  let options = Arg.align
    [ ( "-d"
      , Arg.String Env.set_debug_mode
      , "FLAGS enables debugging for all given flags:
      q : (quiet)    disables all warnings
      n : (notice)   notifies about which symbol or rule is currently treated
      o : (module)   notifies about loading of an external module (associated
                     to the command #REQUIRE)
      c : (confluence) notifies about information provided to the confluence
                     checker (when option -cc used)
      u : (rule)     provides information about type checking of rules
      t : (typing)   provides information about type-checking of terms
      r : (reduce)   provides information about reduction performed in terms
      m : (matching) provides information about pattern matching" )
    ; ( "-v"
      , Arg.Unit (fun () -> Env.set_debug_mode "montru")
      , " Verbose mode (equivalent to -d 'montru')" )
    ; ( "-q"
      , Arg.Unit (fun () -> Env.set_debug_mode "q")
      , " Quiet mode (equivalent to -d 'q')" )
    ; ( "-e"
      , Arg.Set export
      , " Generates an object file (\".dko\")" )
    ; ( "-nc"
      , Arg.Clear Errors.color
      , " Disables colors in the output" )
    ; ( "-stdin"
      , Arg.String (fun n -> run_on_stdin := Some(n))
      , "MOD Parses standard input using module name MOD" )
    ; ( "-version"
      , Arg.Unit (fun () -> Format.printf "Dedukti %s@." Version.version)
      , " Prints the version number" )
    ; ( "-coc"
      , Arg.Set Typing.coc
      , " Allows to declare a symbol whose type contains Type in the
          left-hand side of a product (useful for the Calculus of Construction)" )
    ; ( "-I"
      , Arg.String Basic.add_path
      , "DIR Adds the directory DIR to the load path" )
    ; ( "-ccs"
      , Arg.Set Typing.fail_on_unsatisfiable_constraints
      , " Forbids rules with unsatisfiable constraints" )
    ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
      , " Normalizes all terms printed in error messages" )
    ; ( "-cc"
      , Arg.String Confluence.set_cmd
      , "CMD Set the external confluence checker command to CMD" )
    ; ( "-nl"
      , Arg.Unit (fun _ -> ())
      , " [DEPRECATED] Allow non left-linear rewriting rules (default behavior now)" )
    ; ( "--beautify"
      , Arg.Set beautify
      , " Pretty printer. Print on the standard output" )]
  in
  let usage = Format.sprintf "Usage: %s [OPTION]... [FILE]...
Type checks the given Dedukti FILE(s).
For more information see https://github.com/Deducteam/Dedukti.
Available options:" Sys.argv.(0) in
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
      Parse_channel.handle md (mk_entry !beautify md) stdin;
      if not !beautify
      then Errors.success "Standard input was successfully checked.\n"
  with
  | Env.EnvError (l,e) -> Errors.fail_env_error l e
  | Sys_error err  -> Errors.fail_sys_error err
