open Kernel.Basic

type token =
  | UNDERSCORE of loc
  | TYPE of loc
  (* Update *)
  | KW_DEF of loc
  | KW_DEFAC of loc
  | KW_DEFACU of loc
  | KW_THM of loc
  | KW_INJ of loc
  | KW_PRV of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | QID of (loc * mident * ident)
  | NAME of (loc * mident)
  | REQUIRE of (loc * mident)
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | ID of (loc * ident)
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | EQUAL
  | ARROW
  | QUESTION
  (* New pragma system *)
  | PRAGMA_END of loc
  | PRAGMA_EVAL of loc
  | PRAGMA_INFER of loc
  | PRAGMA_CHECK of loc
  | PRAGMA_CHECKNOT of loc
  | PRAGMA_ASSERT of loc
  | PRAGMA_ASSERTNOT of loc
  | PRAGMA_PRINT of loc
  | PRAGMA_GDT of loc
  | ASSERT of loc
  | VDASH
  (* End of pragma system *)
  | STRING of string
