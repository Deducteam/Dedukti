open Basic

type name = ident list * ident

type _ty =
  | VarTy of ident
  | Arrow of _ty * _ty
  | OpType of name * _ty list
  | Bool

type ty =
  | ForallK of ident * ty
  | Type of _ty

type _term =
  | Forall of ident * _ty * _term
  | Impl of _term * _term
  | VarTerm of ident * _ty * _term list
  | Const of name * _ty * _term list
  | Lam of ident * ty * _term

type term =
  | ForallT of ident * term
  | Term of _term

type proof

type obj =
  | Cst of name * ty * term option
  | TyOp of name * _ty list
  | Thm of name * term * proof option
