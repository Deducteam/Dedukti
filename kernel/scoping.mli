(** Scope managmement: from preterms to terms. *)
open Types

val scope_term : context -> preterm -> term

val scope_pattern : context -> prepattern -> pattern
