open Cfg

module type S =
sig
  type t

  val mk_constraint : cstr -> unit

  val infos : unit -> unit

  val export : unit -> t

  val import : t -> unit

  val to_set : t -> ConstraintsSet.t
end

val s : (module S) ref

module Naive : functor (S:Export.SOLVER) -> S

val to_handler : (module Export.SOLVER) -> string -> (module S)

(*
type var

type univ =
  | Prop
  | Type of int

type constraints = private
  | Univ of var * univ
  | Eq of var * var
  | Max of var * var * var
  | Succ of var * var
  | Rule of var * var * var

val extract_universe : Signature.t -> Term.term -> var

val add_constraint_prop : var -> unit

val add_constraint_type : var -> univ -> unit

val add_constraint_eq   : var -> var -> unit

val add_constraint_succ : var -> var -> unit

val add_constraint_max  : var -> var -> var -> unit

val add_constraint_rule : var -> var -> var -> unit

module ConstraintsSet : Set.S with type elt = constraints

val export : unit -> ConstraintsSet.t

val import : ConstraintsSet.t -> unit

val info : ConstraintsSet.t -> string

val string_of_var : var -> string

val var_of_ident : Basic.ident -> var

val var_of_univ  : univ -> var

val term_of_univ : univ -> Term.term
*)
