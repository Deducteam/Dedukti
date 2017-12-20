open Basic
open Term

type command =
  (* Reduction *)
  | Whnf     of term
  | Hnf      of term
  | Snf      of term
  | Whnf_n_steps of int*term
  | Hnf_n_steps  of int*term
  | Snf_n_steps  of int*term
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
