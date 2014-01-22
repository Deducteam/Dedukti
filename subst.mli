
open Types

val shift               : int -> int -> term -> term

val psubst_l            : (int*(term Lazy.t) list) -> int -> term -> term

val subst               : term -> term -> term

val subst_q             : (int*term) -> int -> term -> term

val subst_meta          : int -> (int*term) list -> term -> term

val subst_meta_p        : (int*term) list -> pattern -> pattern
