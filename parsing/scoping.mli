open Kernel
open Basic
open Term

exception Scoping_error of loc * string

(** Scope managmement: from preterms to terms. *)

val scope_term : mident -> typed_context -> Preterm.preterm -> term
val scope_rule : mident -> Preterm.prule -> Rule.untyped_rule
