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
  ("-d"    , Arg.Int Checker.set_debug_level,   "Level of verbosity" ) ;
  ("-e"    , Arg.Set Checker.export,            "Create a .dko" ) ;
  ("-nc"   , Arg.Clear Errors.color,             "Disable colored output" ) ;
  ("-stdin", Arg.Set run_on_stdin,              "Use standart input" ) ;
  ("-r"    , Arg.Set Signature.ignore_redecl,         "Ignore redeclaration" ) ;
  ("-version", Arg.Unit Version.print_version,  "Version" ) ;
  ("-coc", Arg.Set Typing.coc,               "Typecheck the Calculus of Construction" ) ;
  ("-autodep", Arg.Set Signature.autodep  ,
   "Automatically handle dependencies (experimental)") ;
  ("-I"    , Arg.String Basics.add_path,        "Add a directory to load path");
  ("-errors-in-snf", Arg.Set Errors.errors_in_snf, "Normalize the types in error messages");
  ("-confluence-checker", Arg.String Confluence.initialize,
        "Set the external confluence checker")
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
