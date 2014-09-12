(** Scope managmement: from preterms to terms. *)
open Term
open Rule

val scope_term : context -> preterm -> term

val scope_rule : prule -> rule
