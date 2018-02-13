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


type is_opaque = bool

type entry =
  | Prelude of loc * mident
  | Declaration of loc * ident * Signature.staticity * term
  | Definition of loc * ident * is_opaque * term option * term
  | Rules of Rule.untyped_rule list
  | Command of loc * command
  | Ending
