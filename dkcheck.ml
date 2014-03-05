open Types

(* *** Parsing *** *)

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
            Global.fail (mk_loc line cnum) "Unexpected token '%s'." tok
        end
    | EndOfFile -> ()

(* *** Main *** *)

let print_version _ =
  Global.print_std "Dedukti v%s" Global.version

let args = [
  ("-d"    , Arg.Set_int Global.debug_level, "Level of verbosity" ) ;
  ("-e"    , Arg.Set Global.export,          "Create a .dko" ) ;
  ("-nc"   , Arg.Clear Global.color,         "Disable colored output" ) ;
  ("-stdin", Arg.Set Global.run_on_stdin,    "Use standart input" ) ;
  ("-r"    , Arg.Set Global.ignore_redecl,   "Ignore redeclaration" ) ;
  ("-version", Arg.Unit print_version,       "Version" ) ; 
  ("-autodep", Arg.Set Global.autodep  ,
   "Automatically handle dependencies (experimental)") ]

let run_on_file file =
  let input = open_in file in
    Global.file := file ;
    (Global.debug 1) (mk_loc 1 1) "Processing file '%s' ... " file ;
    parse (Lexing.from_channel input) ;
    Global.success "File '%s' was successfully checked." file

let _ =
  try
    begin
      Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
      if !Global.run_on_stdin then ( 
        Global.debug 1 (mk_loc 1 1) "Processing standard input ...\n" ;
        parse (Lexing.from_channel stdin) ;
        Global.success "Standard input was successfully checked.\n" )
    end
  with
    | Sys_error err             -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
    | Exit                      -> exit 3
