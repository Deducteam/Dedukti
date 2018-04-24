type t

val set_checking  : bool -> unit

val set_solving   : bool -> unit

val set_debug     : int -> unit

val make          : unit -> t

val get_signature : t -> Signature.t
