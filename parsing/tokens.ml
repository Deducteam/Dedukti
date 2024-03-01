open Kernel.Basic

type token =
  | TYPE of loc
  | KW_DEF of loc
  | KW_DEFAC of loc
  | KW_DEFACU of loc
  | KW_THM of loc
  | KW_INJ of loc
  | KW_PRV of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | ID of (loc * ident)
  | QID of (loc * mident * ident)
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | EQUAL
  | ARROW
  | NAME of loc
  | REQUIRE of loc
  | EVAL of loc
  | INFER of loc
  | CHECK of loc
  | ASSERT of loc
  | CHECKNOT of loc
  | ASSERTNOT of loc
  | PRINT of loc
  | GDT of loc
  | STRING of string
  | PRAGMA of (loc * string)
