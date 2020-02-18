open Term
open Basic

val d_SR : Debug.flag

val srfuel : int ref
(** This parameters indicates how much time, substitution followed by reduction
    is applied on the type of the rhs of a rule to check that the rule preserves
    typing. *)

module SRChecker(R:Reduction.S) :
sig
  type lhs_typing_cstr
  (** Representation of LHS typing constraints *)

  val empty : lhs_typing_cstr
  (** No constraints *)

  val get_subst : lhs_typing_cstr -> Exsubst.ExSubst.t
  (** Retrieve extended substitution *)

  val get_unsat : lhs_typing_cstr -> cstr option
  (** If no instance of the LHS is typable,
      output a witness that the rule cannot be triggered *)

  val convertible  : Signature.t -> lhs_typing_cstr -> int -> term -> term -> bool
  (** [convertible sg c depth t u] is true if the constraints [c] ensures that
      [t] and [u], considered both under [depth] abstractions, are convertible *)

  val compile_cstr : Signature.t -> cstr list -> lhs_typing_cstr
  (** Transforms a list of constraints (ie equality between terms under abstractions),
      to a [lhs_typing_cstr]. *)

  val optimize     : Signature.t -> lhs_typing_cstr -> lhs_typing_cstr
  (** Equality inferred while assuming that the LHS is well-typed are put
      in normal form. *)
end
