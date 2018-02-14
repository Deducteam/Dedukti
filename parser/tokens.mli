open Basic

type token =
  | UNDERSCORE  of loc
  | TYPE        of loc
  | KW_DEF      of loc
  | KW_THM      of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | QID         of ( loc * mident * ident )
  | NAME        of ( loc * mident )
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
  | ARROW
  | WHNF        of loc
  | HNF         of loc
  | SNF         of loc
  | INFER       of loc
  | INFERSNF    of loc
  | CONV        of loc
  | CHECK       of loc
  | PRINT       of loc
  | GDT         of loc
  | REQUIRE     of (loc * mident)
  | OTHER       of ( loc * string )
  | STRING      of string
  | INT         of int
