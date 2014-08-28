open Types

type matrix

val mk_matrix : rule list -> matrix
val width : matrix -> int
val pop : matrix -> matrix option
val choose_column : matrix -> int option

val specialize : matrix -> int -> case -> matrix
val partition : matrix -> int -> case list
val default : matrix -> int -> matrix option

type line = private
    { l_rhs:term ; l_eqs:(term*term) list ; l_esize:int ; l_ctx:ctx_loc; }

val first : matrix -> line
