(** Scope managmement: from preterms to terms. *)
val name        : Basic.ident ref
val scope_term : Rule.typed_context -> Preterm.preterm -> Term.term
val scope_rule : Preterm.prule -> Rule.untyped_rule * Preterm.ruletype
