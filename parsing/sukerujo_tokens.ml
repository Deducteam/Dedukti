open Kernel.Basic

include Tokens

type token +=
  | LEFTLST     of loc
  | RIGHTLST    of loc
  | RECORD
  | CHAR        of ( loc * char )
  | STRING      of ( loc * string )
  | NUM         of ( loc * string )
  | INT         of int
