(** Scope managmement: from preterms to terms. *)
val name        : Basic.mident ref
val scope_term : ?mctx:(Basic.ident * int) list -> Rule.typed_context -> Preterm.preterm -> Term.term
val scope_rule : Preterm.prule -> Rule.untyped_rule


val scope_mtype : Term.mctx -> Preterm.pmtype -> Term.mtype

val scope_mterm : Term.mctx -> (Basic.loc * Basic.ident) list -> Preterm.pmterm -> Term.mterm
