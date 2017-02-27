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
  | NEWMODULE      of ( loc * ident )
  | ENDMODULE   of ( loc )
  | QID         of ( loc * ident list * ident )
  | NAME        of ( loc * ident )
  | VERYLONGARROW
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
  | INFER       of loc
  | CONV        of loc
  | CHECK       of loc
  | PRINT       of loc
  | GDT         of loc
  | OTHER       of ( loc * string )
  | CHAR        of ( loc * char )
  | STRING      of ( loc * string )
  | NUM         of ( loc * string )
