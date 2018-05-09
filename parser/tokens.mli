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
  | LONGARROW
  | LEFTLST     of loc
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
  | STEP        of loc
  | NSTEPS      of loc
  | INFER       of loc
  | INFERSNF    of loc
  | CONV        of loc
  | CHECK       of loc
  | PRINT       of loc
  | GDT         of loc
  | REQUIRE     of (loc * mident)
  | OTHER       of ( loc * string )
  | CHAR        of ( loc * char )
  | STRING      of ( loc * string )
  | NUM         of ( loc * string )
  | INT         of int
