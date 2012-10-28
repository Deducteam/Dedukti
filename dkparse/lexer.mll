{
  open Parser
  open Types

  let ln = ref 1

  let mk_loc lexbuf = (!ln,0) (*FIXME*) 
  (*
  let mk_loc lexbuf = 
          let curr = lexbuf.Lexing.lex_curr_p                   in
          let line = curr.Lexing.pos_lnum                       in
          let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                (line,cnum) *)
}

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let qid = id '.' id

rule token = parse
  | [' ' '\t']          { token lexbuf  }
  | '\n'                { incr ln ; token lexbuf }
  | "(;"                { comment lexbuf}
  | '.'                 { DOT           }
  | ','                 { COMMA         }
  | ':'                 { COLON         }
  | '['                 { LEFTSQU       }
  | ']'                 { RIGHTSQU      }
  | '{'                 { LEFTBRA       }
  | '}'                 { RIGHTBRA      }
  | '('                 { LEFTPAR       }
  | ')'                 { RIGHTPAR      }
  | "-->"	        { LONGARROW     }
  | "->"	        { ARROW         }
  | "=>"	        { FATARROW      }
  | "Type"	        { TYPE          }
  | qid as s            { QID s         } 
  | id  as s            { ID (s,mk_loc lexbuf) } 
  | _   as s		{ raise ( Error (LexingError(String.make 1 s,mk_loc lexbuf)) ) }
  | eof		        { raise End_of_file }

 and comment = parse 
  | ";)"                { token lexbuf          }
  | '\n'                { incr ln; comment lexbuf }
  | _                   { comment lexbuf        }
  | eof		        { raise End_of_file     }
  
