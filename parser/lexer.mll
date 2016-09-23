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

  let builtins = Builtins.modname

  let list_of_modules c s =
      let rec aux s l =
      	  try
		let i = String.index s c in
		let md = String.sub s 0 i in
		let s' = String.sub s (i+1) ((String.length s)-(String.length md)-1) in
		aux s' (hstring md::l)
	  with Not_found -> l
      in
      List.rev (aux s [])

}

let space       = [' ' '\t' '\r']
let modname     = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let ident       = ['a'-'z' 'A'-'Z' '0'-'9' '_' '+' '-' '*' '/' '=' '<' '>' '!' '?' '\'' ]+
let capital     = ['A'-'Z']+
let non_neg_num = ['1'-'9']['0'-'9']*

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
  | "def"      { KW_DEF ( get_loc lexbuf )       }
  | "thm"      { KW_THM ( get_loc lexbuf )       }
  | "#NAME" space+ (modname as md)
  { NAME (get_loc lexbuf , hstring md) }
  | "#NEWMODULE" space+ (modname as md)
  { NEWMODULE (get_loc lexbuf , hstring md) }
  | "#ENDMODULE"{ ENDMODULE (get_loc lexbuf) }
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
  | ((modname '.')+ as mds) (ident as id)
  { QID (get_loc lexbuf , list_of_modules '.' mds, hstring id) }
  | non_neg_num as s
  { NUM (get_loc lexbuf, s) }
  | '\'' (_ as c) '\''
  { CHAR ( get_loc lexbuf, c) }
  | ident  as id
  { ID  ( get_loc lexbuf , hstring id ) }
  | "\"\""
  { QID (get_loc lexbuf , [builtins], hstring "string_nil") }
  | '"' { string (Buffer.create 42) lexbuf }
  | _   as s
  { Errors.fail (get_loc lexbuf) "Unexpected characters '%s'." (String.make 1 s) }
  | eof { EOF }

 and comment = parse
  | ";)" { token lexbuf          }
  | '\n' { new_line lexbuf ; comment lexbuf }
  | _    { comment lexbuf        }
  | eof	 { Errors.fail (get_loc lexbuf) "Unexpected end of file."  }

and string buf = parse
  | '\\' (_ as c)
  { Buffer.add_char buf '\\'; Buffer.add_char buf c; string buf lexbuf }
  | '\n'
  { Lexing.new_line lexbuf ; Buffer.add_char buf '\n'; string buf lexbuf }
  | '"'
  { STRING (get_loc lexbuf, Buffer.contents buf) }
  | _ as c
  { Buffer.add_char buf c; string buf lexbuf }
  | eof
  { Errors.fail (get_loc lexbuf) "Unexpected end of file." }
