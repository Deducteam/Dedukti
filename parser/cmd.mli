open Basic
open Term

type command =
  (* Reduction *)
  | Whnf of term
  | Hnf of term
  | Snf of term
  | OneStep of term
  (*Typing*)
  | Conv of term*term
  | Inhabit of term*term
  | ConvNot of term*term
  | InhabitNot of term*term
  | Infer of term
  | AssertConv of term*term
  | AssertInhabit of term*term
  | AssertConvNot of term*term
  | AssertInhabitNot of term*term
  (* Misc *)
  | Gdt of ident option*ident
  | Print of string
  | Other of string*term list

val mk_command : loc -> command -> unit

val print_command : Format.formatter -> command -> unit
