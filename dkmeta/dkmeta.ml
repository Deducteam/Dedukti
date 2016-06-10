open Term

let run_on_stdin        = ref false

module P = Parser.Make(Checker)

let parse lb =
  try
    P.prelude Lexer.token lb ;
    while true do P.line Lexer.token lb done
  with
    | Tokens.EndOfFile -> ()
    | P.Error       -> Errors.fail (Lexer.get_loc lb)
                         "Unexpected token '%s'." (Lexing.lexeme lb)

let args = [
  ("-v"    , Arg.Set Checker.verbose, "Verbose mode" ) ;
  ("-d"    , Arg.Set Basics.debug_mode,   "Debug mode" ) ;  
  ("-e"    , Arg.Set Checker.export,            "Create a .dko" ) ;
  ("-stdin", Arg.Set run_on_stdin,              "Use standart input" ) ;
  ("-r"    , Arg.Set Signature.ignore_redecl,         "Ignore redeclaration" ) ;
  ("-version", Arg.Unit Version.print_version,  "Version" ) ;
  ("-autodep", Arg.Set Signature.autodep  ,
   "Automatically handle dependencies (experimental)") ;
  ("-I"    , Arg.String Basics.add_path,        "Add a directory to load path");
  ("-errors-in-snf", Arg.Set Errors.errors_in_snf, "Normalize the types in error messages");
  ("-cc", Arg.String Confluence.set_cmd, "Set the external confluence checker");
  ("-nl", Arg.Set Dtree.allow_non_linear, "Allow non left-linear rewrite rules");
  ("-snf-only-meta", Arg.Set Checker.only_meta, "Normalize only using meta-rules");
  ("-apply-on-rules", Arg.Set Checker.apply_on_rules, "Normalize inside regular rules")
]

let run_on_file file =
  let input = open_in file in
    Basics.debug "Processing file '%s'..." file;
    parse (Lexing.from_channel input) ;
    Errors.success "File '%s' was successfully checked." file;
    close_in input

let _ =
  try
    begin
      Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
      if !run_on_stdin then (
        parse (Lexing.from_channel stdin) ;
        Errors.success "Standard input was successfully checked.\n" )
    end
  with
    | Sys_error err             -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
    | Exit                      -> exit 3
