open Basic
open Term

type command =
  (* Reduction *)
  | Whnf     of term
  | Hnf      of term
  | Snf      of term
  | OneStep  of term
  | NSteps   of int*term
  (*Typing*)
  | Conv     of bool*bool*term*term
  | Inhabit  of bool*bool*term*term
  | Infer    of term
  | InferSnf of term
  (* Misc *)
  | Gdt      of mident option*ident
  | Require  of mident
  | Print    of string
  | Other    of string*term list
