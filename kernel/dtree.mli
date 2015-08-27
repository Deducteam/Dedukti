open Basics
open Rule

val allow_non_linear : bool ref

type dtree_error =
  | BoundVariableExpected of pattern
  | VariableBoundOutsideTheGuard of Term.term
  | NotEnoughArguments of loc*ident*int*int*int
  | HeadSymbolMismatch of loc*ident*ident
  | ArityMismatch of loc*ident
  | UnboundVariable of loc*ident*pattern
  | AVariableIsNotAPattern of loc*ident
  | DistinctBoundVariablesExpected of loc*ident
  | NonLinearRule of rule2

val to_rule_infos : rule2 -> (rule_infos,dtree_error) error

(** Compilation of rewrite rules into decision trees. *)
val of_rules : rule_infos list -> (int*dtree,dtree_error) error
