open Types

type matrix

val mk_matrix : rule list -> matrix
val width : matrix -> int
val pop : matrix -> matrix option

val get_first_rhs : matrix -> term
val get_first_cstr : matrix -> (term*term) list
val get_first_mtch : matrix -> mtch

val choose_column : matrix -> int option
val specialize : matrix -> int -> case -> matrix
val partition : matrix -> int -> case list
val default : matrix -> int -> matrix option
