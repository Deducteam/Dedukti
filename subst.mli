
open Types

val shift : int -> int -> term -> term

val psubst_l : (int*(term Lazy.t) list) -> int -> term -> term

val subst : term -> term -> term

val subst_q : (int*term) -> int -> term -> term

val subst_pt : partial_term -> partial_term -> partial_term

val meta_subst          : int -> (int*partial_term) list -> partial_term -> partial_term

val meta_subst_pattern  : (int*partial_term) list -> pattern -> pattern
