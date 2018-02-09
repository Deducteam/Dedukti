open Basic
open Term

type command =
  (* Reduction *)
  | Reduce   of Reduction.red_strategy * term
  | Nsteps   of Reduction.red_strategy * int * term
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
