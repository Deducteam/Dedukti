(** {2 Dependencies} *)

(** This module aims to provide helpers to compute dependencies
   between Dedukti terms. There are two kind of dependencies which are
   handled:

   1. Dependencies between modules

   2. Dependencies between names

   For each of those dependencies, two flavors are allowed:

   - [reverse] allows to compute the reverse relation

   - [transitive] allows to compute the transitive closure of the
   relation   
*)

(** Type of dependencies for an entry. *)
type deps = {modules : Kernel.Basic.MidentSet.t; names : Kernel.Basic.NameSet.t}

(** [dep_of_entry ?(strict=true) md entry] computes the dependencies
   [deps] of [entry] assuming the module is [md]. If [strict], then we
   have:

    1. md ∉ deps.modules

    2. name ∉ deps.names where [name] is the name of the entry (if it
   applies). *)
val dep_of_entry :
  ?strict:bool -> Kernel.Basic.mident -> Parsers.Entry.entry -> deps

(** General type to handle dependencies for a list of entries. *)
type t

(** [empty ?(reverse=false) ?(transitive=false) ()] builds an empty
   data-structre. If [reverse], then dependencies will be added in a
   backward way. If [transitive], dependencies will be added
   transitively (this can be costly).      
*)
val empty : ?reverse:bool -> ?transitive:bool -> unit -> t

module type S = sig
  (** Element for which dependencies are computed. *)
  type elt

  (** Set containing those elements. *)
  module Set : Set.S with type elt = elt

  (** [add t elt deps] adds the dependency [elt -> deps] into [t]. Or
     the opposite relation if [t.reverse] is [true]. This transitive
     closure is computed if [t.transitive] is [true]. *)
  val add : t -> elt -> Set.t -> t

  (** [sort t] sorts the dependencies of [t] with respect to the
     dependency relation. Return an error if this dependency relation
     is not irreflexive. This can happen if [add md deps] was called
     such that [md ∈ deps] or that [md] appears in the transitive
     closure. *)
  val sort : t -> (elt list, [`Circular of elt * elt list]) Result.t

  (** [get t elt] allows to get the dependencies of [elt] contained in [t]. *)
  val get : t -> elt -> Set.t
end

(** Handle modules dependencies. *)
module Modules : S with type elt = Kernel.Basic.mident

(** Handle names dependencies. This can be costly in practice,
   especially if [transitive=true]. *)
module Names : S with type elt = Kernel.Basic.name
