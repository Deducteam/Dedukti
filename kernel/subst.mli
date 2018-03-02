(** Substitutions using DeBruijn indices. *)

open Term

exception UnshiftExn

(** [shift i t] shifts every De Bruijn indices in [t] by [i]. *)
val shift : int -> term -> term

(** [unshift i t] shifts every De Bruijn indices in [t] by -[i].
    Raises [UnshiftExn] when [t] contains De Bruijn variables of indices {i k < }[i]. *)
val unshift : int -> term -> term

(** [psubst_l l k t] substitutes the first {i n} De Bruijn variables
    (with {i n} = [length l]) in [t] with the corresponding term in [l].
    Unshifts {i n} times the free variables with greater indices. *)
val psubst_l : (term Lazy.t) Basic.LList.t -> term -> term

(** [subst t u] substitutes the first free De Bruijn variable with [u] in [t].
    Unshifts the other free variables. *)
val subst : term -> term -> term

(** [subst_n n y t] substitutes the [n]-th free De Bruijn variable in [t] with
    a fresh variable named [y] becoming the first free variable in [t].
    All others free variables are shifted by one preventing index collision. *)
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
  val pp : t Basic.printer
  val fold : (int -> (Basic.ident*term) -> 'b -> 'b) -> t -> 'b -> 'b
  val iter : (int -> (Basic.ident*term) -> unit) -> t -> unit
end
