(** Substitutions usind DeBruijn indices. *)
open Types

val shift               : int -> term -> term

val psubst_l            : (int*(term Lazy.t) list) -> int -> term -> term
(** Parallel substitution of lazy terms. *)

val subst               : term -> term -> term
(** Substitution *)
