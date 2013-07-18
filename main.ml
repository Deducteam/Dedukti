
open Types

(* Error Msgs *)

let error e str = 
  Global.print ("\n\027[31m["^e^"]\027[m " ^ str ^ "\n");
  exit 1 

(* Parsing *)

let parse lb = 
  try
      Parser.top Lexer.token lb
  with 
    | Parser.Error       -> 
        begin
          let curr = lb.Lexing.lex_curr_p in
          let line = string_of_int (curr.Lexing.pos_lnum) in
          let column = string_of_int (curr.Lexing.pos_cnum - curr.Lexing.pos_bol) in
          let tok = Lexing.lexeme lb in
            raise (ParserError ("Parsing error near '" ^ tok ^ "' (line:"^line^"; column:"^column^")")) 
        end

(* ... *)

let run_on_stdin _ =
  Global.print (" -- Processing standard input ...\t") ;
  Global.print_v "\n";
  parse (Lexing.from_channel stdin) ;
  Global.print ("\027[32m[DONE]\027[m\n") ;
  Env.export_and_clear ()
            
let run_on_file file =
  let input = open_in file in
    Global.print (" -- Processing file '" ^ file ^ "' ...\t") ;
    Global.print_v "\n";
    parse (Lexing.from_channel input) ;
    Global.print ("\027[32m[DONE]\027[m\n") ;
    Env.export_and_clear ()

(* Args *)

let args = [
        ("-q"    , Arg.Set Global.quiet                 , "Quiet"               ) ;
        ("-v"    , Arg.Clear Global.quiet               , "Verbose"             ) ;
        ("-e"    , Arg.Set Global.export                , "Create a .dko"       ) ;
        ("-stdin", Arg.Unit run_on_stdin                , "Use standart input"  ) 
]

let _ =  
  try 
    Arg.parse args run_on_file "Usage: dkcheck [options] files"  
  with 
    | Sys_error err     -> error "System Error"  err
    | LexerError err    -> error "Lexing Error"  err
    | ParserError err   -> error "Pasing Error"  err
    | TypingError err   -> error "Typing Error"  err
    | EnvError err      -> error "Scoping Error" err
