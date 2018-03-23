type var

type univ =
  | Prop
  | Type of int

type constraints =
  | Univ of var * univ
  | Eq of var * var
  | Max of var * var * var
  | Succ of var * var
  | Rule of var * var * var

val generate_constraints : Signature.t -> Term.term -> Term.term -> bool
(** generate_constraints [sg] [l] [r] returns [true] if some constraints has been generated *)

module ConstraintsSet : Set.S with type elt = constraints

val export : unit -> ConstraintsSet.t

val info : ConstraintsSet.t -> string

val string_of_var : var -> string

val is_matching : bool ref

val var_of_ident : Basic.ident -> var

val term_of_univ : univ -> Term.term
