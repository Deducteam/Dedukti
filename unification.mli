open Types

type 'a substitution = (int*'a) list

val unify_t : (term*term) list -> (term substitution) option
val unify_p : (pattern*pattern) list -> (pattern substitution) option
