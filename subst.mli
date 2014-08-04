(** Substitutions usind DeBruijn indices. *)
open Types

val psubst : term subst -> term -> term
(** Substitution *)

val psubst_l : term Lazy.t subst -> term -> term
(** Substitution of lazy terms *)

val subst : term -> var:Var.t -> by:term -> term
(** [subst t ~var ~by] replaces all occurrences of [var] by [by] in the term [t] *)
