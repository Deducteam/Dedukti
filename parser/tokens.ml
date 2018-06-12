open Basic

type token =
  | UNDERSCORE  of loc
  | TYPE        of loc
  | RIGHTLST    of loc
  | KW_DEF      of loc
  | KW_THM      of loc
  | TYPEOF
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | RECORD
  | QID         of ( loc * mident * ident )
  | NAME        of ( loc * mident )
  | REQUIRE    of ( loc * mident )
  | LONGARROW
  | LEFTLST     of loc
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | ID         of ( loc * ident )
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | CCOLON
  | EQUAL
  | ARROW
  | EVAL       of loc
  | INFER      of loc
  | CHECK      of loc
  | ASSERT     of loc
  | CHECKNOT   of loc
  | ASSERTNOT  of loc
  | PRINT      of loc
  | GDT of loc
  | CHAR        of ( loc * char )
  | STRING      of ( loc * string )
  | NUM         of ( loc * string )
  | INT         of int
