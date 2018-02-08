{
  open Basic
  open Lexing
  open Tokens
  open Format

  exception EndOfFile

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
		aux s' (mk_mident md::l)
	  with Not_found -> l
      in
      List.rev (aux s [])
}

let space       = [' ' '\t' '\r']
let modname     = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let ident       = ['a'-'z' 'A'-'Z' '0'-'9' '_' '+' '-' '*' '/' '=' '<' '>' '!' '?' '\'' ]+
let mident      = ident
let capital     = ['A'-'Z']+
let non_neg_num = ['1'-'9']['0'-'9']*

let const = "nat" | "0" | "S" | "char" | "char_of_nat" | "string" | "string_cons" | "list" | "nil" | "cons"

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
  { NAME (get_loc lexbuf , mk_mident md) }
  | "#NEWMODULE" space+ (modname as md)
  { NEWMODULE (get_loc lexbuf , mk_mident md) }
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
  { QID (get_loc lexbuf , list_of_modules '.' mds, mk_ident id) }
  | non_neg_num as s
  { NUM (get_loc lexbuf, s) }
  | const  as id
  { QID (get_loc lexbuf , [builtins], mk_ident id) }
  | '\'' (_ as c) '\''
  { CHAR ( get_loc lexbuf, c) }
  | ident  as id
  { ID  ( get_loc lexbuf , mk_ident id ) }
  | "\"\""
  { QID (get_loc lexbuf , [builtins], mk_ident "string_nil") }
  | "#REQUIRE" space+ (mident as md) {REQUIRE (get_loc lexbuf, mk_mident md)}
  | '#' (capital as cmd) { OTHER (get_loc lexbuf, cmd) }
  | '#' (non_neg_num  as i  ) { INT   (int_of_string i) }
  | mident as md '.' (ident as id)
  { QID ( get_loc lexbuf , [mk_mident md] , mk_ident id ) }
  | ident  as id
  { ID  ( get_loc lexbuf , mk_ident id ) }
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
