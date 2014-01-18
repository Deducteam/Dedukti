open Types

val pattern_of_term     : term -> pattern

val get_top_pattern_with_constraints : loc -> int -> term -> ( int * ident * pattern array * (term*term) list)

val resolve_type        : loc -> term -> (term*term) list -> term
