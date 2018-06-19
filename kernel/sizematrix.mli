open Term
open Rule

type cmp = Min1 | Zero | Infi

val cmp_to_string : cmp -> string

type matrix =
  { w   : int             ; (* Number of argument of callee *)
    h   : int             ; (* Number of argument of caller *)
    tab : cmp array array   (* The matrix of size h*w *)
  }

val prod : matrix -> matrix -> matrix

val decreasing : matrix -> bool

val subsumes : matrix -> matrix -> bool

val matrix_of_lists : int -> pattern list -> int -> term list -> int -> matrix

val term2rule : rule_infos -> term -> rule_infos
  
