(** Scope managmement: from preterms to terms. *)
val scope_term : Term.context -> Preterm.preterm -> Term.term
val scope_rule : Preterm.prule -> Rule.rule
