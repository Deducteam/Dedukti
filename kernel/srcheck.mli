open Term

val srfuel : int ref

module SRChecker(R:Reduction.S) :
sig
  type t
  val  empty : t
  val get_subst : t -> Exsubst.ExSubst.t
  val get_unsat : t -> cstr option
  val convertible  : Signature.t -> t -> int -> term -> term -> bool
  val compile_cstr : Signature.t -> cstr list -> t
  val optimize     : Signature.t -> t -> t
end
