open Kernel
open Basic
open Term

type is_opaque    = bool
type is_assertion = bool
type should_fail  = bool

type test =
  | Convert of term * term
  | HasType of term * term

type entry =
  | Decl  of loc * ident * Signature.scope * Signature.staticity * term
  | Def   of loc * ident * Signature.scope * is_opaque * term option * term
  | Rules of loc * Rule.partially_typed_rule list
  | Eval  of loc * Reduction.red_cfg * term
  | Check of loc * is_assertion * should_fail * test
  | Infer of loc * Reduction.red_cfg * term
  | Print of loc * string
  | DTree of loc * mident option * ident
  | Name  of loc * mident
  | Require of loc * mident

let loc_of_entry = function
  | Decl(lc,_,_,_,_)
  | Def(lc,_,_,_,_,_)
  | Rules(lc,_)
  | Eval(lc,_,_)
  | Infer(lc,_,_)
  | Check(lc,_,_,_)
  | DTree(lc,_,_)
  | Print(lc,_)
  | Name(lc,_)
  | Require(lc,_)    -> lc
