{
  open Basic
  open Lexing
  open Tokens
  open Format

  let loc_of_pos pos = mk_loc (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol)

  let get_loc lexbuf = loc_of_pos lexbuf.lex_start_p

  let prerr_loc lc =
  let (l,c) = of_loc lc in
    eprintf "line:%i column:%i " l c

  let fail lc fmt =
    eprintf "%s"  "parsing error: ";
    prerr_loc lc;
    kfprintf (fun _ -> pp_print_newline err_formatter () ; raise Exit) err_formatter fmt
}

let space   = [' ' '\t' '\r']
let mident = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let ident   = ['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '?']['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '?' '\'' ]*
let capital = ['A'-'Z']+

rule token = parse
  | space       { token lexbuf  }
  | '\n'        { new_line lexbuf ; token lexbuf }
  | "(;"        { comment lexbuf}
  | '.'         { DOT           }
  | ','         { COMMA         }
  | ':'         { COLON         }
  | "=="        { EQUAL         }
  | '['         { LEFTSQU       }
  | ']'         { RIGHTSQU      }
  | '{'         { LEFTBRA       }
  | '}'         { RIGHTBRA      }
  | '('         { LEFTPAR       }
  | ')'         { RIGHTPAR      }
  | "-->"       { LONGARROW     }
  | "->"        { ARROW         }
  | "=>"        { FATARROW      }
  | ":="        { DEF           }
  | "?"         { WHEN          }
  | "&&"        { AND           }
  | "_"         { UNDERSCORE ( get_loc lexbuf ) }
  | "Type"      { TYPE       ( get_loc lexbuf ) }
  | "def"       { KW_DEF     ( get_loc lexbuf ) }
  | "thm"       { KW_THM     ( get_loc lexbuf ) }
  | "#NAME"    space+ (mident as md) { NAME    (get_loc lexbuf , mk_mident md) }
  | "#REQUIRE" space+ (mident as md) { REQUIRE (get_loc lexbuf , mk_mident md) }
  | "#EVAL"     { EVAL       ( get_loc lexbuf ) }
  | "#INFER"    { INFER      ( get_loc lexbuf ) }
  | "#CHECK"    { CHECK      ( get_loc lexbuf ) }
  | "#CHECKNOT" { CHECKNOT   ( get_loc lexbuf ) }
  | "#ASSERT"   { ASSERT     ( get_loc lexbuf ) }
  | "#ASSERTNOT"{ ASSERTNOT  ( get_loc lexbuf ) }
  | "#PRINT"    { PRINT      ( get_loc lexbuf ) }
  | "#GDT"      { GDT        ( get_loc lexbuf ) }
  | mident as md '.' (ident as id)
  { QID ( get_loc lexbuf , mk_mident md , mk_ident id ) }
  | ident  as id
  { ID  ( get_loc lexbuf , mk_ident id ) }
  | '"' { string (Buffer.create 42) lexbuf }
  | _   as s
  { fail (get_loc lexbuf) "Unexpected characters '%s'." (String.make 1 s) }
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
  { STRING (Buffer.contents buf) }
  | _ as c
  { Buffer.add_char buf c; string buf lexbuf }
  | eof
  { fail (get_loc lexbuf) "Unexpected end of file." }
