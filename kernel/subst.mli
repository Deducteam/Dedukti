(** Substitutions usind DeBruijn indices. *)
open Term

val shift               : int -> term -> term

val psubst_l            : (term Lazy.t) LList.t -> int -> term -> term
(** Parallel substitution of lazy terms. *)

val subst               : term -> term -> term
(** Substitution *)
