open Types

let run_on_stdin        = ref false

module P = Parser.Make(Checker)

let parse lb =
  try
    P.prelude Lexer.token lb ;
    while true do P.line Lexer.token lb done
  with
    | P.Error   ->
        begin
          let start = lb.Lexing.lex_start_p in
          let line = start.Lexing.pos_lnum  in
          let cnum = start.Lexing.pos_cnum - start.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            Print.fail (mk_loc line cnum) "Unexpected token '%s'." tok
        end
    | Tokens.EndOfFile -> ()

let print_version _ =
  Printf.printf "Dedukti v%s" Version.version

let args = [
  ("-d"    , Arg.Int Checker.set_debug_level, "Level of verbosity" ) ;
  ("-e"    , Arg.Set Checker.export,          "Create a .dko" ) ;
  ("-nc"   , Arg.Clear Print.color,         "Disable colored output" ) ;
  ("-stdin", Arg.Set run_on_stdin,    "Use standart input" ) ;
  ("-r"    , Arg.Set Env.ignore_redecl,   "Ignore redeclaration" ) ;
  ("-version", Arg.Unit print_version,       "Version" ) ;
  ("-autodep", Arg.Set Env.autodep  ,
   "Automatically handle dependencies (experimental)") ]

let run_on_file file =
  let input = open_in file in
    parse (Lexing.from_channel input) ;
    Print.success "File '%s' was successfully checked." file;
    close_in input

let _ =
  try
    begin
      Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
      if !run_on_stdin then (
        parse (Lexing.from_channel stdin) ;
        Print.success "Standard input was successfully checked.\n" )
    end
  with
    | Sys_error err             -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
    | Exit                      -> exit 3
