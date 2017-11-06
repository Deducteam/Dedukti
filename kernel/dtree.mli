open Basic
open Rule
open Matching

(** {2 Decision Trees} *)

(** Arguments of a pattern may be the following:
    - a constant
    - a variable
    - a lambda expression *)
type case =
  | CConst of int*ident*ident*bool
  (** [size] [m] [v] [ac] where [size] is the number of arguments expected for the constant [m.v] and [ac] is true iif the constant is a definable AC(U) symbol. *)
  | CDB    of int*int
  (** [size] [i] where size is the number of *static* arguments expected for the bounded variable [i] *)
  | CLam (** Just a lambda term *)


(** Type of decision trees *)
type dtree =
  | Fetch of int * case * dtree * dtree option
  (** Fetch [i] [case] [tree_suc] [tree_def] assumes the [i]-th argument of a pattern is a
   * flattened AC symbols and checks that it contains a term that can be matched with the given
   * case.
   * If so then look at the corresponding tree, otherwise/afterwise, look at the default tree *)
  | ACEmpty of int * dtree * dtree option
  (** ACEmpty [i] [tree_suc] [tree_def] assumes the [i]-th argument of a pattern is a
   * flattened AC symbols and checks that it is now empty. *)
  | Switch  of int * (case*dtree) list * dtree option
  (** Switch [i] [(case_0,tree_0) ; ... ; (case_n, tree_n)] [tree_opt] tests
   * whether the [i]-th argument of a pattern can be matched with one of the cases of the list.
   * If so then look at the corresponding tree, otherwise, look at the default tree *)
  | Test    of int matching_problem * constr list * Term.term * dtree option
  (** Test [pb] [cstrs] [te] [tree_opt] are the leaves of the tree.
    * Checks that each problem can be solved such that constraints are satisfied.
    * If it does then return a local context for the term [te]. *)

val pp_dtree : Format.formatter -> dtree -> unit
val pp_case : Format.formatter -> case -> unit

(** [md] [v] [tree] is the dtree associated to the constant [md].[v] *)
type rw = ident * ident * dtree

val pp_rw : Format.formatter -> rw -> unit

(** {2 Error} *)

type dtree_error =
  | HeadSymbolMismatch of loc * ident * ident
  | ArityMismatch of loc * ident
  | ArityInnerMismatch of loc * ident * ident

(** Compilation of rewrite rules into decision trees. *)
val of_rules : (ident->ident->bool) -> rule_infos list -> (dtree, dtree_error) error
