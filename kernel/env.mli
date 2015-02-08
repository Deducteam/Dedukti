(** Global Environment *)

open Basics
open Term

val init                : ident -> unit
val get_type            : loc -> ident -> ident -> term
val get_name            : unit -> ident
val declare             : loc -> ident -> term -> unit
val define              : loc -> ident -> term -> term -> unit
val add_rules           : Rule.rule list -> unit
val get_dtree           : loc -> ident -> ident -> Signature.dtree_or_def
val export              : unit -> unit
