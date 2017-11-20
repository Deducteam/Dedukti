open Basic
open Term

val mk_prelude     : loc -> mident -> unit

val mk_declaration : loc -> ident -> Signature.staticity -> term -> unit

val mk_definition  : loc -> ident -> term option -> term -> unit

val mk_opaque      : loc -> ident -> term option -> term -> unit

val mk_rules       : Rule.untyped_rule list -> unit

val mk_command     : loc -> Cmd.command -> unit

val mk_meta_definition : Basic.loc -> Basic.ident -> Term.mtype -> Term.mterm -> unit

val mk_ending      : unit -> unit
