module UVar:
sig

  type uvar = Basic.ident

  val is_uvar : Term.term -> bool

  val extract_uvar : Term.term -> Basic.ident

  val fresh : unit -> uvar

end

module Mapping:
sig

  type index

  type t

  val to_index : UVar.uvar -> index

  val from_index : index -> UVar.uvar

  val string_of_index : index -> string

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

module Constraints:
sig


  type constraints =
    | Univ of Mapping.index * ReverseCiC.univ
    | Eq of Mapping.index * Mapping.index
    | Succ of Mapping.index * Mapping.index
    | Rule of Mapping.index * Mapping.index * Mapping.index

  val generate_constraints : Term.term -> Term.term -> bool
  (** generate_constraints [l] [r] returns [true] if some constraints has been generated *)

  module ConstraintSet : Set.S with type elt = constraints

  val export : unit -> ConstraintSet.t

  val info : unit -> string
end

module Elaboration:
sig

  val elaboration : Signature.t -> Term.term -> Term.term

end

module Reconstruction :
sig

  type model = UVar.uvar -> Term.term

  val reconstruction : model -> Term.term -> Term.term

end
module Log:
sig

  val set_log_file : string -> unit

  val log_file : unit -> string

  val log : unit -> bool

  val append : string -> unit

  val close : unit -> unit
end
