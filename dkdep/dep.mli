val out                 : out_channel ref

val init                : Basic.mident -> unit

val mk_entry            : Parser.entry -> unit

val finalize            : unit -> unit

val filename            : string ref

val verbose             : bool ref

val sorted              : bool ref

val sort                : unit -> string list
