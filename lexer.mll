{
  open Types
  open Parser

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
  | "#NAME"                     { NAME }
  | "#IMPORT"                   { IMPORT }
  | "#NORMALIZE"                { NORM }
  | "Type"	                { TYPE          }
  | id as s1 '.' (id as s2)     { QID (mk_loc lexbuf,Global.hstring s1,Global.hstring s2) } 
  | id  as s                    { ID (mk_loc lexbuf,Global.hstring s) } 
  | _   as s		        { let (l,c) = mk_loc lexbuf in
                                  raise (LexerError ("Lexing error near '"^String.make 1 s^"' (line:"^string_of_int l^";column:"^string_of_int c^")." )) }
  | eof		                { EOF }

 and comment = parse 
  | ";)"                { token lexbuf          }
  | '\n'                { Lexing.new_line lexbuf ; comment lexbuf }
  | _                   { comment lexbuf        }
  | eof		        { raise (LexerError "End_of_file_in_comment") }
  
