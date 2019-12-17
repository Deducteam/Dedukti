open Term
open Basic

type Debug.flag += D_SRChecking

val srfuel : int ref

module SRChecker(R:Reduction.S) :
sig
  type t
  (** Representation of LHS typing constraints *)

  val empty : t
  (** No constraints *)

  val get_subst : t -> Exsubst.ExSubst.t
  (** Retrieve extended substitution *)

  val get_unsat : t -> cstr option
  val convertible  : Signature.t -> t -> int -> term -> term -> bool
  val compile_cstr : Signature.t -> cstr list -> t
  val optimize     : Signature.t -> t -> t
end
