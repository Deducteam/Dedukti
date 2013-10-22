
open Types

type gst =
  | Decl  of term*(int*gdt) option 
  | Def   of term*term

val init                : StringH.key -> unit

val import              : StringH.key -> unit 

val export_and_clear    : unit -> unit

val get_global_symbol   : StringH.key -> StringH.key -> gst

val get_global_type     : StringH.key -> StringH.key -> term

val get_global_rw       : StringH.key -> StringH.key -> (int*gdt) option

val add_decl            : StringH.key -> term -> unit

val add_def             : StringH.key -> term -> term -> unit

val add_rw              : StringH.key -> (int*gdt) -> unit
