{
  open Types
  open Lexing

  let get_loc lexbuf =
          let start = lexbuf.lex_start_p                in
          let line = start.pos_lnum                     in
          let cnum = start.pos_cnum - start.pos_bol     in
                mk_loc line cnum
}

let space   = [' ' '\t']
let modname = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let ident   = ['a'-'z' 'A'-'Z' '0'-'9' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '?' '\'' ]*

rule token = parse
  | space       { token lexbuf  }
  | '\n'        { Lexing.new_line lexbuf ; token lexbuf }
  | "(;"        { comment lexbuf}
  | '.'         { DOT           }
  | ','         { COMMA         }
  | ':'         { COLON         }
  | '['         { LEFTSQU       }
  | ']'         { RIGHTSQU      }
  | '{'         { LEFTBRA       }
  | '}'         { RIGHTBRA      }
  | '('         { LEFTPAR       }
  | ')'         { RIGHTPAR      }
  | "-->"	{ LONGARROW     }
  | "->"	{ ARROW         }
  | "=>"	{ FATARROW      }
  | ":="	{ DEF           }
  | "~="	{ EQUIV          }
  | "_"         { UNDERSCORE ( get_loc lexbuf ) }
  | "Type"      { TYPE ( get_loc lexbuf )       }
  | "#NAME" space+ (modname as md)      { NAME (get_loc lexbuf , hstring md)     }
  | "#IMPORT" space+ (modname as md)    { IMPORT (get_loc lexbuf , hstring md)   }
  | "#ASSERT"                           { ASSERT (get_loc lexbuf) }
  | modname as md '.' (ident as id)     { QID ( get_loc lexbuf , hstring md , hstring id ) }
  | ident  as id                        { ID  ( get_loc lexbuf , hstring id ) }
  | _   as s		                { raise ( LexerError ( get_loc lexbuf , "Unexpected characters '" ^ String.make 1 s ^ "'." ) ) }
  | eof		                        { EOF }

 and comment = parse
  | ";)"                { token lexbuf          }
  | '\n'                { new_line lexbuf ; comment lexbuf }
  | _                   { comment lexbuf        }
  | eof		        { raise ( LexerError ( get_loc lexbuf , "Unexpected end of file." ) ) }
