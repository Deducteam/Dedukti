
open Types

val shift               : int -> int -> term -> term

val psubst_l            : (int*(term Lazy.t) list) -> int -> term -> term

val subst               : term -> term -> term

val subst_q             : (int*term) -> int -> term -> term

val subst_meta          : (int*term) list -> term -> term

val subst_pattern       : (int*pattern) list -> pattern -> pattern

val subst_pattern2      : (int*term) list -> pattern -> pattern
