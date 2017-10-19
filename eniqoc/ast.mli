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

type declaration = Axiom of name * term | Parameter of name * term

type definition = Theorem of name * term * term | Constant of name * term * term

type obj =
  {
    depends: declaration list;
    definition: definition list
  }

type module_id = string

type ast =
  {
    name:module_id;
    prelude:module_id list;
    obj:obj
  }
