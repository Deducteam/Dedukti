
open Types

type gst = private
  | Decl  of term * (int*gdt*rule list) option
  | Def   of term * term
  | Static of term

val init                : ident -> unit

val export_and_clear    : unit -> unit

val get_global_symbol   : loc -> ident -> ident -> gst

val get_global_type     : loc -> ident -> ident -> term

val get_global_rw       : loc -> ident -> ident -> (int*gdt*rule list) option

val is_neutral          : loc -> ident -> ident -> bool

val add_decl            : loc -> ident -> term -> unit

val add_static          : loc -> ident -> term -> unit

val add_def             : loc -> ident -> term -> term -> unit

val add_rw              : loc -> ident -> rule list -> unit

val foreach_rule        : (rule -> unit) -> unit

val foreach_module_rule : (rule -> unit) -> unit
