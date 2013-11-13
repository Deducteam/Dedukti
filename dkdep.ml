open Types

module P = Parser.Make(Dep) 

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
            Global.warning (mk_loc l c) ("Unexpected token '" ^ tok ^ "'." )
        end
    | EndOfFile -> ()

let run_on_file file =
  let input = open_in file in
    parse (Lexing.from_channel input)

let args = [ ("-o", Arg.String Global.set_out                , "Output file"  ) ]

(* Main *)

let _ =  
  try 
    Arg.parse args run_on_file "Usage: dkdep [options] files"  
  with 
    | Sys_error err             -> Global.warning dloc  err
    | LexerError (lc,err)       -> Global.warning lc err
    | ParserError (lc,err)      -> Global.warning lc err


