type token =
  | EOF
  | AT
  | DOT
  | COMMA
  | COLON
  | ARROW
  | FATARROW
  | LONGARROW
  | DEF
  | UNDERSCORE of (Types.loc)
  | NAME
  | IMPORT
  | LEFTPAR
  | RIGHTPAR
  | LEFTBRA
  | RIGHTBRA
  | LEFTSQU
  | RIGHTSQU
  | TYPE
  | ID of (Types.lvar)
  | QID of (Types.lid)

val top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
