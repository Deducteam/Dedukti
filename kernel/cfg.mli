type t

val set_checking   : bool -> unit

val set_solving    : bool -> unit

val set_debug      : int -> unit

val get_signature  : unit -> Signature.t

val get_checking   : unit -> bool

val add_name       : Basic.name -> unit

val add_fmt        : Basic.mident -> string -> unit

val add_uvars      : Basic.name -> Basic.ISet.t -> unit

val get_uvars      : Basic.name -> Basic.ISet.t

val incr_cpt_uvars : unit -> unit
