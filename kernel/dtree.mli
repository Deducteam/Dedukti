open Rule
val to_rule_infos : rule -> rule_infos
(** Compilation of rewrite rules into decision trees. *)
val of_rules : rule_infos list -> int * dtree
