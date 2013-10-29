open Types

module P = Parser.Make(Mmt) 

let parse lb = 
  try
      P.prelude Lexer.token lb ;
      while true do P.line Lexer.token lb done
  with 
    | P.Error       -> 
        begin
          let curr = lb.Lexing.lex_curr_p in
          let l = curr.Lexing.pos_lnum in
          let c = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lb in
            raise (ParserError ( mk_loc l c , "Unexpected token '" ^ tok ^ "'." ) ) 
        end
    | EndOfFile -> ()

let run_on_file file =
  let input = open_in file in
    Global.eprint (" -- Processing file '" ^ file ^ "' ...\t") ;
    parse (Lexing.from_channel input) ;
    Global.eprint ("\027[32m[DONE]\027[m\n")

let args = [ ("-o", Arg.String Global.set_out                , "Output file"  ) ]

(* Main *)

let _ =  
  try 
    Arg.parse args run_on_file "Usage: dkcheck [options] files"  
  with 
    | Sys_error err             -> Global.error dloc "System Error"  err
    | LexerError (lc,err)       -> Global.error lc "Lexing Error"  err
    | ParserError (lc,err)      -> Global.error lc "Parsing Error"  err


