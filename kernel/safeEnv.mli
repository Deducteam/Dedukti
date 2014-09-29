open Basics
open Term

val add_decl            : loc -> ident -> term -> unit
val add_opaque          : loc -> ident -> term -> term option -> unit
val add_def             : loc -> ident -> term -> term option -> unit
val add_rules           : Rule.rule list -> unit
