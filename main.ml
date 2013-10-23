
open Types

(* *** Parsing *** *)

module P = Parser.Make(Checker)

let parse lb = 
  try
      P.top Lexer.token lb
  with 
    | P.Error       -> 
        begin
          let curr = lb.Lexing.lex_curr_p in
          let l = curr.Lexing.pos_lnum in
          let c = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            raise (ParserError ( (l,c) , "Unexpected token '" ^ tok ^ "'." ) ) 
        end

(* *** Input *** *)

          (*
let ascii_art _ =
  Global.print_v 
"==========================================================================
 \\ \\    / /__| |__ ___ _ __  ___  | |_ ___  |   \\ ___ __| |_  _| |_| |_(_)
  \\ \\/\\/ / -_) / _/ _ \\ '  \\/ -_) |  _/ _ \\ | |) / -_) _` | || | / /  _| |
   \\_/\\_/\\___|_\\__\\___/_|_|_\\___|  \\__\\___/ |___/\\___\\__,_|\\_,_|_\\_\\\\__|_|
==========================================================================
"
           *)

let run_on_stdin _ =
  Global.print (" -- Processing standard input ...\t") ;
  Global.print_v "\n";
  parse (Lexing.from_channel stdin) ;
  Global.print ("\027[32m[DONE]\027[m\n") ;
  Env.export_and_clear ()
            
let run_on_file file =
  let input = open_in file in
   (* ascii_art (); *)
    Global.print (" -- Processing file '" ^ file ^ "' ...\t") ;
    Global.print_v "\n";
    parse (Lexing.from_channel input) ;
    Global.print ("\027[32m[DONE]\027[m\n") ;
    Env.export_and_clear ()

(* *** Arguments *** *)

let args = [
        ("-q"    , Arg.Set Global.quiet                 , "Quiet"               ) ;
        ("-v"    , Arg.Clear Global.quiet               , "Verbose"             ) ;
        ("-e"    , Arg.Set Global.export                , "Create a .dko"       ) ;
        ("-stdin", Arg.Unit run_on_stdin                , "Use standart input"  ) ; 
        ("-r"    , Arg.Set Global.raphael               , "Undocumented"  ) 
]

(* *** Main *** *)

let _ =  
  try 
    Arg.parse args run_on_file "Usage: dkcheck [options] files"  
  with 
    | Sys_error err             -> Global.error (0,0) "System Error"  err
    | LexerError (lc,err)       -> Global.error lc "Lexing Error"  err
    | ParserError (lc,err)      -> Global.error lc "Parsing Error"  err
    | TypingError err           -> Global.error (0,0) "Typing Error"  err (*FIXME*)
    | EnvError (lc,err)         -> Global.error lc "Scoping Error" err 
    | PatternError err          -> Global.error (0,0) "Rewrite Error" err (*FIXME*)
