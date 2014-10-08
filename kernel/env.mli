(** Global Environment *)

open Basics
open Term
open Rule

val ignore_redecl       : bool ref
val autodep             : bool ref

type dtree_or_def =
    | DoD_None
    | DoD_Def of term
    | DoD_Dtree of int*dtree

val init                : ident -> unit
val get_type            : loc -> ident -> ident -> term
val get_name            : unit -> ident
val declare             : loc -> ident -> term -> unit
val define              : loc -> ident -> term -> term -> unit
val add_rules           : rule list -> unit
val get_dtree           : loc -> ident -> ident -> dtree_or_def
val export              : unit -> unit

val get_all_rules       : string -> (string*frule list) list
