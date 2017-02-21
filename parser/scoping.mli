(** Scope managmement: from preterms to terms. *)
val name        : Basic.ident ref
val scope_term : Rule.context -> Preterm.preterm -> Term.term
val scope_rule : Preterm.prule -> Rule.rule
