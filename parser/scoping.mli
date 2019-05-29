open Basic
open Term

(** Scope managmement: from preterms to terms. *)
val scope_term : mident -> typed_context -> Preterm.preterm -> term
val scope_rule : mident -> Preterm.prule -> Rule.part_typed_rule
