open Term
open Rule

type matrix

val mk_matrix : rule list -> matrix
val width : matrix -> int
val pop : matrix -> matrix option

val get_first_term : matrix -> term
val get_first_constraints : matrix -> (term*term) list
val get_first_pre_context : matrix -> pre_context

val choose_column : matrix -> int option
val specialize : matrix -> int -> case -> matrix
val partition : matrix -> int -> case list
val default : matrix -> int -> matrix option
