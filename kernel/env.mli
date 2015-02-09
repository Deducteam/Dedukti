(** Global Environment *)

open Basics
open Term

val init        : ident -> unit
val get_type    : loc -> ident -> ident -> term
val get_name    : unit -> ident
val get_dtree   : loc -> ident -> ident -> Signature.dtree_or_def
val export      : unit -> unit

val declare     : loc -> ident -> term -> unit
val define      : loc -> ident -> term -> term option -> unit
val define_op   : loc -> ident -> term -> term option -> unit
val add_rules   : Rule.rule list -> unit

val infer       : term -> term
val check       : term -> term -> bool

val hnf         : term -> term
val whnf        : term -> term
val snf         : term -> term
val one         : term -> term option
val are_convertible : term -> term -> bool

