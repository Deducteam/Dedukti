(** Dedukti's main functionnalities. *)
open Term

val export         : bool ref
val set_debug_level : int -> unit

val mk_prelude     : loc -> ident -> unit

val mk_declaration : loc -> ident -> preterm -> unit

val mk_definition  : loc -> ident -> preterm option -> preterm -> unit

val mk_opaque      : loc -> ident -> preterm option -> preterm -> unit

val mk_rules       : Rule.prule list -> unit

val mk_command     : loc -> Cmd.command -> unit

val mk_ending      : unit -> unit
