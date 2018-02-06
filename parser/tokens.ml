open Basic

exception EndOfFile

type token =
  | UNDERSCORE  of loc
  | TYPE        of loc
  | KW_DEF      of loc
  | KW_THM      of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | QID         of ( loc * ident * ident )
  | NAME        of ( loc * ident )
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | ID          of ( loc * ident )
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | CCOLON
  | EQUAL
  | ARROW
  | WHNF        of loc
  | HNF         of loc
  | SNF         of loc
  | STEP        of loc
  | INFER       of loc
  | CHECK       of loc
  | ASSERT      of loc
  | CHECKNOT    of loc
  | ASSERTNOT   of loc
  | PRINT       of loc
  | GDT         of loc
  | OTHER       of ( loc * string )
  | STRING      of string
