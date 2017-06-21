open Basic

type name = ident * ident

type term =
  | Type
  | Prop
  | Lam of ident * term * term
  | App of term * term
  | Forall of ident * term * term
  | Impl of term * term
  | Var of ident
  | Const of name

type obj =
  | Axiom of name * term
  | Parameter of name * term
  | Constant of name * term * term
  | Theorem of name * term * term
