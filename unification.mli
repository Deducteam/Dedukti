open Types

type 'a substitution = (int*'a) list

type uty =
  | MGU         of term substitution
  | UPrefix     of term substitution
  | NoUnifier
(*
type unif_error = NoUnifier | NoWHNF (*| TooComplex*)
 *)
val unify : (term*term) list -> uty
(*
val unify_p : (pattern*pattern) list -> (pattern substitution) option
 *)
