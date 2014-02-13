open Types

val mk_prelude          : loc -> ident -> unit

val mk_declaration      : loc -> ident -> preterm -> unit

val mk_definition       : loc -> ident -> preterm option -> preterm -> unit

val mk_opaque           : loc -> ident -> preterm option -> preterm -> unit

val mk_rules            : prule list -> unit

val mk_command           : loc -> string -> preterm list -> unit

val mk_ending           : unit -> unit

