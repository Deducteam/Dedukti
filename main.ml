
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
            raise (ParserError ( mk_loc l c , "Unexpected token '" ^ tok ^ "'." ) ) 
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
  Global.eprint (" -- Processing standard input ...\t") ;
  Global.vprint "\n";
  parse (Lexing.from_channel stdin) ;
  Global.eprint ("\027[32m[DONE]\027[m\n") ;
  Env.export_and_clear ()
            
let run_on_file file =
  let input = open_in file in
   (* ascii_art (); *)
    Global.eprint (" -- Processing file '" ^ file ^ "' ...\t") ;
    Global.vprint "\n";
    parse (Lexing.from_channel input) ;
    Global.eprint ("\027[32m[DONE]\027[m\n") ;
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
    | Sys_error err             -> Global.error dloc "System Error"  err
    | LexerError (lc,err)       -> Global.error lc "Lexing Error"  err
    | ParserError (lc,err)      -> Global.error lc "Parsing Error"  err
    | TypingError (lc,err)      -> Global.error lc "Typing Error"  err
    | EnvError (lc,err)         -> Global.error lc "Scoping Error" err 
    | PatternError (lc,err)     -> Global.error lc "Rule Error" err 
