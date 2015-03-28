(** Substitutions using DeBruijn indices. *)
open Term

val shift               : int -> term -> term
exception UnshiftExn
val unshift             : int -> term -> term

val psubst_l            : (term Lazy.t) Basics.LList.t -> int -> term -> term
(** Parallel substitution of lazy terms. *)

val subst               : term -> term -> term
(** [subst te u] substitutes the deBruijn indice [0] with [u] in [te]. *)

module S :
sig
  type t
  val identity : t
  val add : t -> int -> term -> t option
  val apply : t -> term -> term
  val merge : t -> t -> t
end
