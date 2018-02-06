open Basic
open Term

type command =
  (* Reduction *)
  | Whnf of term
  | Hnf of term
  | Snf of term
  | OneStep of term
  | Conv of term*term
  (*Typing*)
  | Check of term*term
  | Infer of term
  | Assert of term*term
  (* Misc *)
  | Gdt of ident option*ident
  | Print of string
  | Other of string*term list

val mk_command : loc -> command -> unit

val print_command : Format.formatter -> command -> unit
