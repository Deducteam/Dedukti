open Basic

exception EndOfFile

type token =
  | UNDERSCORE  of loc
  | TYPE        of loc
  | KW_DEF      of loc
  | KW_THM      of loc
  | KW_REC      of loc
  | KW_LET      of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | VDASH
  | QID         of ( loc * ident * ident )
  | NAME        of ( loc * ident )
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | ID          of ( loc * ident )
  | FATARROW
  | LONGFATARROW
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
  | OTHER       of ( loc * string )
  | STRING      of string
  | INT         of int
