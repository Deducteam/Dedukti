open Basics
open Rule

type dtree_error =
(* Print.fail (get_loc_pat p) "The pattern '%a' is not a bound variable." Pp.pp_pattern p *)
  | BoundVariableExpected of pattern
(* Print.fail (get_loc t) "The term '%a' contains a variable bound outside the brackets." Pp.pp_term t *)
  | VariableBoundOutsideTheGuard of Term.term
(*   Print.fail l "The variable '%a' must be applied to at least %i argument(s)." pp_ident id nb_args.(n-k) *)
  | NotEnoughArguments of loc*ident*int
(*   Print.fail r2.l "Unexpected head symbol '%a' \ (expected '%a')." pp_ident r2.id pp_ident r1.id *)
  | HeadSymbolMismatch of loc*ident*ident
(*   Print.fail r2.l "All the rewrite rules for \ the symbol '%a' should have the same arity." pp_ident r1.id *)
  | ArityMismatch of loc*ident
(*  Print.fail l "The variables '%a' is not bounded in '%a'." pp_ident x Pp.pp_pattern p *)
  | UnboundVariable of loc*ident*pattern
(*  Print.fail l "A variable is not a valid pattern." *)
  | AVariableIsNotAPattern of loc*ident

val to_rule_infos : rule -> (rule_infos,dtree_error) error
(** Compilation of rewrite rules into decision trees. *)
val of_rules : rule_infos list -> (int*dtree,dtree_error) error
