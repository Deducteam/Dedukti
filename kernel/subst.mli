(** Substitutions using DeBruijn indices. *)
open Term

val shift               : int -> term -> term
exception UnshiftExn
val unshift             : int -> term -> term

val psubst_l            : (term Lazy.t) Basics.LList.t -> int -> term -> term
(** Parallel substitution of lazy terms. *)

val subst               : term -> term -> term
(** [subst te u] substitutes the deBruijn indice [0] with [u] in [te]. *)

val subst_n : int -> Basics.ident -> term -> term
(** [subst_n n y t] replaces x[n] by y[0] and shift by one*)

module S :
sig
  type t
  val identity : t
  val add : t -> Basics.ident -> int -> term -> t option
  val apply : t -> term -> int -> term
  val merge : t -> t -> t
  val is_identity : t -> bool
  val mk_idempotent : t -> t
  val pp : out_channel -> t -> unit
end
