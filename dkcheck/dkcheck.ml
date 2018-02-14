open Term

let run_on_stdin        = ref false

let export = ref false

let print_version () =
  Printf.printf "Dedukti %s\n%!" Version.version

let default_mident = ref None

let set_default_mident md = default_mident := Some md

let args = [
  ("-v"      , Arg.Set    Checker.verbose        , "Verbose mode" ) ;
  ("-d"      , Arg.Int    Basic.set_debug_mode   , "Debug mode" ) ;
  ("-e"      , Arg.Set    export                 , "Create a .dko" ) ;
  ("-nc"     , Arg.Clear  Errors.color           , "Disable colored output" ) ;
  ("-stdin"  , Arg.Set    run_on_stdin           , "Use standart input" ) ;
  ("-r"      , Arg.Set    Signature.ignore_redecl, "Ignore redeclaration" ) ;
  ("-version", Arg.Unit   print_version          , "Version" ) ;
  ("-coc"    , Arg.Set    Typing.coc             , "Typecheck the Calculus of Construction" ) ;
  ("-autodep", Arg.Set    Signature.autodep      ,
   "Automatically handle dependencies (experimental)") ;
  ("-I"      , Arg.String Basic.add_path         , "Add a directory to load path");
  ("-errors-in-snf",
               Arg.Set    Errors.errors_in_snf   , "Normalize the types in error messages");
  ("-cc"     , Arg.String Confluence.set_cmd     , "Set the external confluence checker");
  ("-nl"     , Arg.Set    Rule.allow_non_linear  , "Allow non left-linear rewrite rules");
  ("-module" , Arg.String set_default_mident     , "Give a default name to the current module");
]



let run_on_file file =
  let input = open_in file in
  Basic.debug 1 "Processing file '%s'..." file;
  let md =  Basic.mk_mident (match !default_mident with None -> file | Some str -> str) in
  Env.init md;
  Confluence.initialize ();
  Parser.handle_channel md Checker.mk_entry input;
  Errors.success "File '%s' was successfully checked." file;
  ( if !export then
    if not (Env.export ()) then
      Errors.fail Basic.dloc "Fail to export module '%a'." Basic.pp_mident (Env.get_name ()) );
  Confluence.finalize ();
  close_in input

let _ =
  try
    begin
      Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
      if !run_on_stdin then (
        let md = Basic.mk_mident (
            match !default_mident with
            | None -> Basic.debug 0 "[Warning] no module name given"; "stdin"
            | Some str -> str)
        in
        Env.init md;
        Parser.handle_channel md Checker.mk_entry stdin;
        Errors.success "Standard input was successfully checked.\n" )
    end
  with
    | Sys_error err -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
    | Exit          -> exit 3
