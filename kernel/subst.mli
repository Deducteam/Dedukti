(** Substitutions using DeBruijn indices. *)
open Term

exception UnshiftExn

(** [shift i t] shifts every deBruijn indices in [t] by [i]. *)
val shift               : int -> term -> term

(** [unshift i t] shifts every deBruijn indices in [t] by [-i]. Raise [UnshiftExn] when
    it is applied to an indices [k] such that k < i. *)
val unshift             : int -> term -> term

(** psubst_l [l] [k] [t] substitutes the i first free variables in [t] by [l_i] when i<n with [n] = length [l] *)
val psubst_l            : (term Lazy.t) Basic.LList.t -> term -> term

(** [subst te u] substitutes the deBruijn indice [0] with [u] in [te]. *)
val subst               : term -> term -> term

(** [subst_n n y t] replaces x[n] by y[0] and shift by one. *)
val subst_n : int -> Basic.ident -> term -> term


module Subst :
sig
  type t
  val identity : t
  val add : t -> Basic.ident -> int -> term -> t option
  val apply : t -> term -> int -> term
  (* val merge : t -> t -> t *)
  val is_identity : t -> bool
  val mk_idempotent : t -> t
  val pp : Format.formatter -> t -> unit
  val fold : (int -> (Basic.ident*term) -> 'b -> 'b) -> t -> 'b -> 'b
  val iter : (int -> (Basic.ident*term) -> unit) -> t -> unit
end
