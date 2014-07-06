
open Types

val init                : ident -> unit

val export_and_clear    : unit -> unit

val get_infos           : loc -> ident -> ident -> rw_infos

val get_type            : loc -> ident -> ident -> term

val add_decl            : loc -> ident -> term -> unit

val add_def             : loc -> ident -> term -> term -> unit

val add_rw              : rule list -> unit
