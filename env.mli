
open Types

type gst =
  | Decl  of term*(int*gdt) option 
  | Def   of term*term

val init                : StringH.key -> unit

val import              : loc -> StringH.key -> unit 

val export_and_clear    : unit -> unit

val get_global_symbol   : loc -> StringH.key -> StringH.key -> gst

val get_global_type     : loc -> StringH.key -> StringH.key -> term

val get_global_rw       : loc -> StringH.key -> StringH.key -> (int*gdt) option

val add_decl            : loc -> StringH.key -> term -> unit

val add_def             : loc -> StringH.key -> term -> term -> unit

val add_rw              : loc -> StringH.key -> (int*gdt) -> unit
