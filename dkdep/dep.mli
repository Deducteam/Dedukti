val out                 : out_channel ref

val mk_entry            : Toplevel.entry -> unit

val filename            : string ref

val verbose             : bool ref

val sorted              : bool ref

val sort                : unit -> string list
