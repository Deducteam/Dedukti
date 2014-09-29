open Basics
open Preterm

val add_decl            : loc -> ident -> preterm -> unit
val add_opaque          : loc -> ident -> preterm -> preterm option -> unit
val add_def             : loc -> ident -> preterm -> preterm option -> unit
val add_rules           : prule list -> unit
