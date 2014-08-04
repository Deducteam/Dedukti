(** Scope managmement: from preterms to terms. *)
open Types

val scope_term : ctx:context -> preterm -> term

val scope_pattern : ctx:context -> prepattern -> pattern
