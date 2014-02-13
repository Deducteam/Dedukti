
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
            raise (ParserError (mk_loc line cnum ,
                                "Unexpected token '" ^ tok ^ "'." ) )
        end
    | EndOfFile -> ()

(* *** Input *** *)

let run_on_stdin _ =
  Global.vprint dloc (lazy " -- Processing standard input ...") ;
  parse (Lexing.from_channel stdin) ;
  Global.print_ok "none"

let run_on_file file =
  let input = open_in file in
    Global.vprint dloc (lazy (" -- Processing file '" ^ file ^ "' ...")) ;
    Global.set_filename file ;
    parse (Lexing.from_channel input) ;
    Global.print_ok file

(* *** Main *** *)

let args = [
        ("-q"    , Arg.Set Global.quiet        , "Quiet"                  ) ;
        ("-v"    , Arg.Clear Global.quiet      , "Verbose"                ) ;
        ("-e"    , Arg.Set Global.export       , "Create a .dko"          ) ;
        ("-nc"   , Arg.Clear Global.color      , "Disable colored output" ) ;
        ("-stdin", Arg.Unit run_on_stdin       , "Use standart input"     ) ;
        ("-unsafe", Arg.Set Global.unsafe_mode , "Unsafe mode"            ) ;
        ("-r"    , Arg.Set Global.raphael      , "Undocumented"           ) ]

let _ =
  try
    Arg.parse args run_on_file ("Usage: "^Sys.argv.(0)^" [options] files")
  with
    | Sys_error err             -> Global.error dloc err
    | LexerError (lc,err)       -> Global.error lc err
    | ParserError (lc,err)      -> Global.error lc err
    | TypingError (lc,err)      -> Global.error lc err
    | EnvError (lc,err)         -> Global.error lc err
    | PatternError (lc,err)     -> Global.error lc err
    | MiscError (lc,err)        -> Global.error lc err
