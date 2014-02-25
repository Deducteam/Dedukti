open Types

type 'a substitution = (int*'a) list
type unif_error = NoUnifier | NoWHNF (*| TooComplex*)

val unify_t : (term*term) list -> (term substitution,unif_error) sum

val unify_p : (pattern*pattern) list -> (pattern substitution) option
