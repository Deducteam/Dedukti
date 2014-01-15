open Types

val pattern_of_term     : term -> pattern

val resolve_type        : loc -> term -> term -> (term*term) list -> term*term
