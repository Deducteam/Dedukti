open Basic
open Term

type command =
  (* Reduction *)
  | Whnf of term
  | Hnf of term
  | Snf of term
  | OneStep of term
  (*Typing*)
  | Conv of bool*bool*term*term
  | Inhabit of bool*bool*term*term
  | Infer of term
  (* Misc *)
  | Gdt of ident option*ident
  | Print of string
  | Other of string*term list

val mk_command : loc -> command -> unit

val print_command : Format.formatter -> command -> unit
