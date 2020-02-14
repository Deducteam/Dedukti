open Term
open Basic

val d_SR : Debug.flag

val srfuel : int ref

module SRChecker(R:Reduction.S) :
sig
  type lhs_typing_cstr
  (** Representation of LHS typing constraints *)

  val empty : lhs_typing_cstr
  (** No constraints *)

  val get_subst : lhs_typing_cstr -> Exsubst.ExSubst.t
  (** Retrieve extended substitution *)

  val get_unsat : lhs_typing_cstr -> cstr option
  val convertible  : Signature.t -> lhs_typing_cstr -> int -> term -> term -> bool
  val compile_cstr : Signature.t -> cstr list -> lhs_typing_cstr
  val optimize     : Signature.t -> lhs_typing_cstr -> lhs_typing_cstr
end
