open Basic
open Term

type command =
  (* Reduction *)
  | Eval     of Env.eval_config * term
  | Conv     of term*term
  (*Typing*)
  | Check    of term*term
  | Infer    of Env.eval_config * term
  (* Misc *)
  | Gdt      of mident option*ident
  | Require  of mident
  | Print    of string
  | Other    of string*term list
