(** Dedukti's main functionnalities. *)
open Basics
open Term

val export         : bool ref
val verbose        : bool ref

type 'a m
type entry
val return         : 'a -> 'a m
val bind           : 'a m -> ('a -> 'b m) -> 'b m
val mk_prelude     : loc -> ident -> entry m
val mk_declaration : loc -> ident -> term -> entry m
val mk_definition  : loc -> ident -> term option -> term -> entry m
val mk_definable   : loc -> ident -> term -> entry m
val mk_opaque      : loc -> ident -> term option -> term -> entry m
val mk_rules       : Rule.rule list -> entry m
val mk_command     : loc -> Cmd.command -> entry m
val mk_Type        : loc -> term m
val mk_DB          : loc -> ident -> int -> term m
val mk_Const       : loc -> ident -> ident -> term m
val mk_Lam         : loc -> ident -> term option -> term -> term m
val mk_App         : term  -> term -> term list -> term m
val mk_Pi          : loc -> ident -> term -> term -> term m
val mk_Arrow       : loc -> term   -> term -> term m
val mk_ending      : entry m -> unit
