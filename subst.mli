(** Substitutions usind DeBruijn indices. *)
open Types

val shift               : int -> term -> term

val contains            : int -> term -> bool
(** [contains k t] returns true iff [t] contains the de Bruijn index [k] *)

val unshift             : int -> term -> term
(** [unshift r t] shifts down all free variables of [t] by [r]. It will
    fail if some variable is captured this way (for instance Lam (db1) cannot
    be unshifted by 2).
    @raise Failure if variables are captured *)

val can_unshift         : int -> term -> bool
(** [can_unshift r t] is [true] iff [t] can be unshifted of [r] safely. *)

val psubst_l            : (term Lazy.t) LList.t -> int -> term -> term
(** Parallel substitution of lazy terms. *)

val subst               : term -> term -> term
(** Substitution *)
