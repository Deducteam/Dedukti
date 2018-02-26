val just_check : bool ref

module UVar:
sig

  type uvar = Basic.ident

  val is_uvar : Term.term -> bool

  val ident_of_uvar : Term.term -> Basic.ident

  val fresh_uvar : Signature.t -> Term.term

  val count : unit -> int
end

module ReverseCiC:
sig

  (** Suppose that all the constants can be found in a module cic.dk *)

  type univ =
    | Prop
    | Type of int

  val term_of_univ : univ -> Term.term

  val z : Basic.name

  val s : Basic.name

  val succ : Basic.name

  val sort : Basic.name

  val rule : Basic.name

  val prop : Basic.name

  val type_ : Basic.name

  val is_prop : Term.term -> bool

  val is_type : Term.term -> bool

  val is_succ : Term.term -> bool

  val is_rule : Term.term -> bool

  val extract_type : Term.term -> int

  val extract_succ : Term.term -> Term.term

  val extract_rule : Term.term -> Term.term * Term.term


end

module type ConstraintsInterface =
sig

  type var

  type constraints =
    | Univ of var * ReverseCiC.univ
    | Eq of var * var
    | Max of var * var * var
    | Succ of var * var
    | Rule of var * var * var

  val generate_constraints : Signature.t -> Term.term -> Term.term -> bool
  (** generate_constraints [l] [r] returns [true] if some constraints has been generated *)

  module ConstraintsSet : Set.S with type elt = constraints

  val export : unit -> ConstraintsSet.t

  val info : ConstraintsSet.t -> string

  val string_of_var : var -> string

  val is_matching : bool ref
end

module Naive:ConstraintsInterface with type var = Basic.ident

module Log:
sig

  val set_log_file : string -> unit

  val log_file : unit -> string

  val append : string -> unit

  val close : unit -> unit
end
