{
  open Basic
  open Lexing
  open Tokens
  open Format


  let get_loc lexbuf =
          let start = lexbuf.lex_start_p                in
          let line = start.pos_lnum                     in
          let cnum = start.pos_cnum - start.pos_bol     in
                mk_loc line cnum

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
let ident   = ['a'-'z' 'A'-'Z' '0'-'9' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '?' '\'' ]*
let capital = ['A'-'Z']+
let number  = ['0'-'9']+

rule token = parse
  | space       { token lexbuf  }
  | '\n'        { new_line lexbuf ; token lexbuf }
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
  | "-->"       { LONGARROW     }
  | "->"        { ARROW         }
  | "=>"        { FATARROW      }
  | ":="        { DEF           }
  | "_"         { UNDERSCORE ( get_loc lexbuf ) }
  | "Type"      { TYPE ( get_loc lexbuf )       }
  | "def"       { KW_DEF ( get_loc lexbuf )       }
  | "thm"       { KW_THM ( get_loc lexbuf )       }
  | "#NAME" space+ (mident as md)
  { NAME (get_loc lexbuf , Name.make_mident md) }
  | "#WHNF"     { WHNF     ( get_loc lexbuf ) }
  | "#HNF"      { HNF      ( get_loc lexbuf ) }
  | "#SNF"      { SNF      ( get_loc lexbuf ) }
  | "#STEP"     { STEP     ( get_loc lexbuf ) }
  | "#NSTEPS"   { NSTEPS   ( get_loc lexbuf ) }
  | "#INFER"    { INFER    ( get_loc lexbuf ) }
  | "#INFERSNF" { INFERSNF ( get_loc lexbuf ) }
  | "#CONV"     { CONV     ( get_loc lexbuf ) }
  | "#CHECK"    { CHECK    ( get_loc lexbuf ) }
  | "#PRINT"    { PRINT    ( get_loc lexbuf ) }
  | "#GDT"      { GDT      ( get_loc lexbuf ) }
  | '#' (capital as cmd) { OTHER (get_loc lexbuf, cmd) }
  | '#' (number  as i  ) { INT   (int_of_string i) }
  | mident as md '.' (ident as id)
  { QID ( get_loc lexbuf , md , id ) }
  | ident  as id
  { ID  ( get_loc lexbuf , id ) }
  | '"' { string (Buffer.create 42) lexbuf }
  | _   as s
  { fail (get_loc lexbuf) "Unexpected characters '%s'." (String.make 1 s) }
  | eof { EOF }

 and comment = parse
  | ";)" { token lexbuf          }
  | '\n' { new_line lexbuf ; comment lexbuf }
  | _    { comment lexbuf        }
  | eof	 { fail (get_loc lexbuf) "Unexpected end of file."  }

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
