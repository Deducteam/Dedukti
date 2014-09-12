(** Compilation of rewrite rules into decision trees. *)
val of_rules : Rule.rule list -> int*Rule.dtree
