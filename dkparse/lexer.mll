{
  open Parser
  open Types

  let mk_loc lexbuf = 
          let curr = lexbuf.Lexing.lex_curr_p                   in
          let line = curr.Lexing.pos_lnum                       in
          let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                (line,cnum) 
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
  | "_"	                        { UNDERSCORE (mk_loc lexbuf)    }
  | "#"	                        { HASH          }
  | "Type"	                { TYPE          }
  | id as s1 '.' (id as s2)     { QID (s1,s2,mk_loc lexbuf)         } 
  | id  as s                    { ID (s,mk_loc lexbuf) } 
  | _   as s		        { raise ( ParsingError (LexerError(String.make 1 s,mk_loc lexbuf)) ) }
  | eof		                { EOF }

 and comment = parse 
  | ";)"                { token lexbuf          }
  | '\n'                { Lexing.new_line lexbuf ; comment lexbuf }
  | _                   { comment lexbuf        }
  | eof		        { raise End_of_file_in_comment }
  
