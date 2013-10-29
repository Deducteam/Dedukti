
open Types

val add_decl    : loc -> ident -> pterm -> unit

val add_def     : loc -> ident -> pterm option -> pterm -> unit

val add_opaque  : loc -> ident -> pterm option -> pterm -> unit

val add_rules   : loc -> ident -> prule list -> unit

