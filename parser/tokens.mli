open Basic

exception EndOfFile

type token =
  | UNDERSCORE  of loc
  | TYPE        of loc
  | RIGHTLST    of loc
  | KW_DEF      of loc
  | KW_THM      of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | RECORD
  | NEWMODULE      of ( loc * mident )
  | ENDMODULE   of ( loc )
  | QID         of ( loc * mident list * ident )
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
