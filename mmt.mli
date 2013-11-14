open Types

val mk_prelude          : loc -> ident -> unit

val mk_require          : loc -> ident -> unit

val mk_declaration      : loc -> ident -> pterm -> unit

val mk_definition       : loc -> ident -> pterm option -> pterm -> unit

val mk_opaque           : loc -> ident -> pterm option -> pterm -> unit

val mk_term             : pterm -> unit

val mk_rules            : prule list -> unit

val mk_ending           : unit -> unit
