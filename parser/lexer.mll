{
  open Basic
  open Lexing
  open Tokens
  open Format

  let loc_of_pos pos = mk_loc (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol)

  let get_loc lexbuf = loc_of_pos lexbuf.lex_start_p

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

  let prerr_loc lc = eprintf "%a " pp_loc lc

  let fail lc fmt =
    eprintf "%s"  "parsing error: ";
    prerr_loc lc;
    kfprintf (fun _ -> pp_print_newline err_formatter () ; raise Exit) err_formatter fmt

  let no_keyword = ref false
}


let space       = [' ' '\t' '\r']
let modname     = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let ident   = ['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '?']['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '?' '\'' ]*
let mident = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
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
  | "=="        { EQUAL         }
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
  | "Record"    { if !no_keyword then ID (get_loc lexbuf, mk_ident "Record") else
                  RECORD        }
  | "_"         { UNDERSCORE ( get_loc lexbuf ) }
  | "Type"      { TYPE ( get_loc lexbuf )       }
  | "def"      { KW_DEF ( get_loc lexbuf )       }
  | "thm"      { KW_THM ( get_loc lexbuf )       }
  | "%typeof"   { TYPEOF }
  | "#NAME"    space+ (mident as md) { NAME    (get_loc lexbuf , mk_mident md) }
  | "#REQUIRE" space+ (mident as md) { REQUIRE (get_loc lexbuf , mk_mident md) }
  | "#EVAL"     { EVAL       ( get_loc lexbuf ) }
  | "#INFER"    { INFER ( get_loc lexbuf ) }
  | "#CHECK"    { CHECK      ( get_loc lexbuf ) }
  | "#CHECKNOT" { CHECKNOT   ( get_loc lexbuf ) }
  | "#ASSERT"   { ASSERT     ( get_loc lexbuf ) }
  | "#ASSERTNOT"{ ASSERTNOT  ( get_loc lexbuf ) }
  | "#PRINT"    { PRINT ( get_loc lexbuf ) }
  | "#GDT"      { GDT ( get_loc lexbuf ) }
  | (mident as md) '.' (ident as id)
  { QID (get_loc lexbuf , mk_mident md, mk_ident id) }
  | non_neg_num as s
  { if !no_keyword then ID (get_loc lexbuf, mk_ident s) else
    NUM (get_loc lexbuf, s)}
  | const  as id
  { if !no_keyword then ID (get_loc lexbuf, mk_ident id) else
    QID (get_loc lexbuf , builtins, mk_ident id) }
  | '\'' (_ as c) '\''
  { CHAR ( get_loc lexbuf, c) }
  | ident  as id
  { ID  ( get_loc lexbuf , mk_ident id ) }
  | "\"\""
  { QID (get_loc lexbuf , builtins, mk_ident "string_nil") }
  | mident as md '.' (ident as id)
  { QID ( get_loc lexbuf , mk_mident md , mk_ident id ) }
  | ident  as id
  { ID  ( get_loc lexbuf , mk_ident id ) }
  | '"' { string (Buffer.create 42) lexbuf }
  | _   as s
  { Errors.fail (get_loc lexbuf) "Unexpected characters '%s'." (String.make 1 s) }
  | eof { EOF }

and comment = parse
  | ";)" { token lexbuf }
  | '\n' { new_line lexbuf ; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { fail (get_loc lexbuf) "Unexpected end of file."  }

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
