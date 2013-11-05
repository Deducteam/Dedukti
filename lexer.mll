{
  open Types
  open Lexing

  let get_loc lexbuf = 
          let start = lexbuf.lex_start_p                in
          let line = start.pos_lnum                     in
          let cnum = start.pos_cnum - start.pos_bol     in
                mk_loc line cnum
}

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
  | [' ' '\t']                  { token lexbuf  }
  | '\n'                        { Lexing.new_line lexbuf ; token lexbuf }
  | "(;"                        { comment lexbuf}
  | '.'                         { DOT           }
  | ','                         { COMMA         }
  | ':'                         { COLON         }
  | '['                         { LEFTSQU       }
  | ']'                         { RIGHTSQU      }
  | '{'                         { LEFTBRA       }
  | '}'                         { RIGHTBRA      } 
  | '('                         { LEFTPAR       }
  | ')'                         { RIGHTPAR      }
  | "-->"	                { LONGARROW     }
  | "->"	                { ARROW         }
  | "=>"	                { FATARROW      }
  | ":="	                { DEF           }
  | "_"	                        { UNDERSCORE ( get_loc lexbuf )    }
  | "#NAME"                     { NAME }
  | "#IMPORT"                   { IMPORT }
  | "#NORMALIZE"                { NORM }
  | "Type"	                { TYPE ( get_loc lexbuf )  }
  | id as s1 '.' (id as s2)     { QID ( get_loc lexbuf , hstring s1 , hstring s2 ) } 
  | id  as s                    { ID ( get_loc lexbuf , hstring s ) } 
  | _   as s		        { raise ( LexerError ( get_loc lexbuf , "Unexpected characters '" ^ String.make 1 s ^ "'." ) ) }
  | eof		                { EOF }

 and comment = parse 
  | ";)"                { token lexbuf          }
  | '\n'                { new_line lexbuf ; comment lexbuf }
  | _                   { comment lexbuf        }
  | eof		        { raise ( LexerError ( get_loc lexbuf , "Unexpected end of file." ) ) }
  
