(** Compilation of rewrite rules into decision trees. *)
val of_rules : Rule.frule list -> int*Rule.dtree
