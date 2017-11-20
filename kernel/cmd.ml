open Basic
open Term

type command =
  (* Reduction *)
  | Whnf     of term
  | Hnf      of term
  | Snf      of term
  | OneStep  of term
  | NSteps   of int*term
  | Conv     of term*term
  (*Typing*)
  | Check    of term*term
  | Infer    of term
  | InferSnf of term
  (* Misc *)
  | Gdt      of mident option*ident
  | Require  of mident
  | Print    of string
  | Other    of string*term list
