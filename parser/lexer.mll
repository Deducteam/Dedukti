{
  open Basics
  open Lexing
  open Tokens
  open Printf

  let get_loc lexbuf =
          let start = lexbuf.lex_start_p                in
          let line = start.pos_lnum                     in
          let cnum = start.pos_cnum - start.pos_bol     in
                mk_loc line cnum

  let chars_read = ref ""
  let add_char c = chars_read := Printf.sprintf "%s%c" !chars_read c

  let flush () = chars_read := ""

  let builtins = Builtins.modname
}

let space       = [' ' '\t']
let modname     = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let ident       = ['a'-'z' 'A'-'Z' '0'-'9' '_' '+' '-' '*' '/' '=' '<' '>' '!' '?' '\'' ]+
let capital     = ['A'-'Z']+
let non_neg_num = ['1'-'9']['0'-'9']*

let const = "nat" | "0" | "S" | "char" | "string" | "string_cons" | "list" | "nil" | "cons"

rule token = parse
  | space       { token lexbuf  }
  | '\n'        { new_line lexbuf ; token lexbuf }
  | "(;"        { comment lexbuf}
  | '.'         { DOT           }
  | ','         { COMMA         }
  | ':'         { COLON         }
  | "[["        { LEFTLST  ( get_loc lexbuf ) }
  | "]]"        { RIGHTLST ( get_loc lexbuf ) }
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
  | "Record"    { RECORD        }
  | "_"         { UNDERSCORE ( get_loc lexbuf ) }
  | "Type"      { TYPE ( get_loc lexbuf )       }
  | "#NAME" space+ (modname as md)
  { NAME (get_loc lexbuf , hstring md) }
  | "#WHNF"     { WHNF ( get_loc lexbuf ) }
  | "#HNF"      { HNF ( get_loc lexbuf ) }
  | "#SNF"      { SNF ( get_loc lexbuf ) }
  | "#STEP"     { STEP ( get_loc lexbuf ) }
  | "#INFER"    { INFER ( get_loc lexbuf ) }
  | "#CONV"     { CONV ( get_loc lexbuf ) }
  | "#CHECK"    { CHECK ( get_loc lexbuf ) }
  | "#PRINT"    { PRINT ( get_loc lexbuf ) }
  | "#GDT"      { GDT ( get_loc lexbuf ) }
  | '#' (capital as cmd)
  { OTHER (get_loc lexbuf, cmd) }
  | modname as md '.' (ident as id)
  { QID ( get_loc lexbuf , hstring md , hstring id ) }
  | non_neg_num as s
  { NUM (get_loc lexbuf, s) }
  | const  as id
  { QID (get_loc lexbuf , builtins, hstring id) }
  | '\'' (_ as c) '\''
  { CHAR ( get_loc lexbuf, c) }
  | ident  as id
  { ID  ( get_loc lexbuf , hstring id ) }
  | "\"\""
  { QID (get_loc lexbuf , builtins, hstring "string_nil") }
  | '"' { flush (); string lexbuf }
  | _   as s
  { Print.fail (get_loc lexbuf) "Unexpected characters '%s'." (String.make 1 s) }
  | eof { EOF }

 and comment = parse
  | ";)" { token lexbuf          }
  | '\n' { new_line lexbuf ; comment lexbuf }
  | _    { comment lexbuf        }
  | eof	 { Print.fail (get_loc lexbuf) "Unexpected end of file."  }

and string = parse
  | '\\' (_ as c) { add_char '\\'; add_char c; string lexbuf }
  | '\n' { Lexing.new_line lexbuf ; add_char '\n'; string lexbuf }
  | '"'  { STRING (get_loc lexbuf, !chars_read) }
  | _ as c { add_char c; string lexbuf }
  | eof	 { Print.fail (get_loc lexbuf) "Unexpected end of file."  }


