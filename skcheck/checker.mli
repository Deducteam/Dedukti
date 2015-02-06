(** Dedukti's main functionnalities. *)
open Basics
open Term

val export         : bool ref
val set_debug_level : int -> unit

val mk_prelude     : loc -> ident -> unit

val mk_declaration : loc -> ident -> term -> unit

val mk_definition  : loc -> ident -> term option -> term -> unit

val mk_opaque      : loc -> ident -> term option -> term -> unit

val mk_rules       : Rule.rule list -> unit

val mk_command     : loc -> Cmd.command -> unit

val mk_ending      : unit -> unit
