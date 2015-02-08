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

type t

val dummy               : t
val make                : ident -> t
val get_name            : t -> ident

val export              : t -> unit
val get_type            : t -> loc -> ident -> ident -> term
val get_dtree           : t -> loc -> ident -> ident -> dtree_or_def
val declare             : t -> loc -> ident -> term -> unit
val define              : t -> loc -> ident -> term -> term -> unit
val add_rules           : t -> Rule.rule list -> unit
