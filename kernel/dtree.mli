open Basic
open Rule

type Debug.flag += D_matching

(** {2 Error} *)

type dtree_error =
  | HeadSymbolMismatch  of loc * name * name
  | ArityInnerMismatch  of loc * ident * ident

exception DtreeError of dtree_error

(** {2 Decision Trees} *)

(** Arguments of a pattern may be the following:
    - a constant
    - a variable
    - a lambda expression
*)
type case =
  | CConst of int * name
  (** [size c] where [size] is the number of *static* arguments expected for the constant [c] *)
  | CDB of int * int
  (** [(size,db_index)] where [size] is the number of *static* arguments expected
      for the bounded variable [db_index] *)
  | CLam  (** A lambda headed term *)

(** An atomic matching problem.
     stack.(pos) ~? X[ DB(args_0), ..., DB(args_n)]
  where X is the variable and the problem is considered under depth abstractions.*)
type atomic_problem =
  {
    pos     : int; (** position of the term to match in the stack. *)
    depth   : int; (** depth of the argument regarding absractions *)
    args_db : int LList.t (** Arguments DB indices (distinct bound variables) *)
  }

(** A matching problem to build a solution context from the stack *)
type matching_problem = atomic_problem LList.t

(** Type of decision trees *)
type dtree =
  | Switch of int * (case*dtree) list * dtree option
  (** [Switch i \[(case_0,tree_0) ; ... ; (case_n, tree_n)\] default_tree]
      tests whether the [i]-th argument in the stack matches with one of the given cases.
      If it does then proceed with the corresponding tree
      Otherwise, branch to the given default tree. *)
  | Test of rule_name * matching_problem * constr list * Term.term * dtree option
  (** [Test name pb cstrs rhs default_tree] are the leaves of the tree.
      Checks that each problem can be solved such that constraints are satisfied.
      If it does then return a local context for the term [rhs]. *)

type t
(** Type mapping arities to decision trees (also called "forest") *)

val empty : t
(** Empty forest *)

val find_dtree : int -> t -> (int * dtree) option
(** [find_dtree ar forest] returns a pair (arity,dtree) in given forest
    such that arity <= ar. Returns [None] when not found. *)

val pp_dtree : dtree printer
(** Printer for a single decision tree. *)

val pp_dforest : t printer
(** Printer for forests of decision trees. *)

val of_rules : rule_infos list -> t
(** Compilation of rewrite rules into decision trees.
    Returns a list of arities and corresponding decision trees.
    Invariant : arities must be sorted in decreasing order.
    (see use case in [state_whnf] in [reduction.ml])
    May raise DtreeError.
*)
