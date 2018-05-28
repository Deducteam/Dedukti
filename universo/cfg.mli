type univ = Var of string | Prop | Type of int | Succ of univ | Max of univ * univ | Rule of univ * univ

type cstr = univ * univ

module ConstraintsSet : Set.S with type elt = cstr

type t

val set_checking    : bool -> unit

val set_solving     : bool -> unit

val set_debug       : int -> unit

val set_signature   : Signature.t -> unit

val get_signature   : unit -> Signature.t

val get_checking    : unit -> bool

val add_name        : Basic.name -> unit

val add_fmt         : Basic.mident -> string -> unit

val add_uvars       : Basic.name -> Basic.ISet.t -> unit

val get_uvars       : Basic.name -> Basic.ISet.t

val univ_max        : unit -> int

val add_constraints : Basic.name -> ConstraintsSet.t -> unit

val get_constraints : Basic.name -> ConstraintsSet.t
