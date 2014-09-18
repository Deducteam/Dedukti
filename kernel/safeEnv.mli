open Term

val add_decl            : loc -> ident -> preterm -> unit
val add_opaque          : loc -> ident -> preterm -> preterm option -> unit
val add_def             : loc -> ident -> preterm -> preterm option -> unit
val add_rules           : Rule.prule list -> unit
