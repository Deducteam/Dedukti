open Term
open Basic
open Rule
open Ac

(** {2 Error} *)

type dtree_error =
  | HeadSymbolMismatch  of loc * name * name
  | ArityInnerMismatch  of loc * ident * ident
  | ACSymbolRewritten   of loc * name * int

exception DtreeError of dtree_error


(** ([n], [vars]) represents the [n]-th variable applied to the [vars] bound variables. *)
type var_p = int * int LList.t

(** {2 Pre-Matching problems} *)

(** Abstract matching problems. This can be instantiated with
    - When building a decision tree ['a = int] refers to positions in the stack
    - When matching against a term, ['a = term Lazy.t] refers to actual terms
*)

(* TODO: add loc to this to better handle errors *)

type 'a eq_problem = int * int LList.t * 'a
(** [(depth, \[x1...xn\], t)] is the higher order
    equational problem: [X\[x1  ... xn\] = t]
    under [depth] lambdas. *)

type 'a ac_problem = int * ac_ident * int * (var_p list) * 'a
  (** [(depth, symb, njoks, vars, terms)]
   *  Represents the flattenned equality under AC symbol [symb] of:
   *  - [njoks] jokers and the given variables [vars]
   *  - The given [terms]
      e.g.
        [ +{ X\[x\] , _, Y\[y,z\] } = +{ f(a), f(y), f(x)} ]
   *)

type pre_matching_problem =
  {
    pm_eq_problems : int eq_problem list array;
    (** For each variable of a rewrite rule (array),
        a list of equational problems under various depths *)
    pm_ac_problems : int ac_problem list;
    (** A list of AC-matching problems under a certain depth *)
    pm_arity       : int array
    (** Constant time access to a variable's arity *)
  }
(** A problem with int indices referencing positions in the stack  *)

val pp_var_type : var_p printer

val pp_eq_problems : string -> 'a printer -> (int * 'a eq_problem list) printer

val pp_ac_problem : 'a printer -> 'a ac_problem printer

(** int matching problem printing function (for dtree). *)
val pp_pre_matching_problem : string -> pre_matching_problem printer


(** {2 Decision Trees} *)

(** Arguments of a pattern may be the following:
    - a constant
    - a variable
    - a lambda expression
*)
type case =
  | CConst of int * name * bool
  (** [(size,name,ac)] where [size] is the number of arguments expected for the
      constant [c] and [ac] is true iff the constant is a definable AC(U) symbol. *)
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
  | Test of rule_name * pre_matching_problem * constr list * term * dtree option
  (** [Test name pb cstrs rhs default_tree] are the leaves of the tree.
      Checks that each problem can be solved such that constraints are satisfied.
      If it does then return a local context for the term [rhs]. *)
  | Fetch of int * case * dtree * dtree option
  (** [Fetch i case tree_suc tree_def] assumes the [i]-th argument of a pattern is a
   * flattened AC symbols and checks that it contains a term that can be matched with the given
   * case.
   * If so then look at the corresponding tree, otherwise/afterwise, look at the default tree *)
  | ACEmpty of int * dtree * dtree option
  (** [ACEmpty i tree_suc tree_def] assumes the [i]-th argument of a pattern is a
   * flattened AC symbols and checks that it is now empty. *)

type t
(** Type mapping arities to decision trees (also called "forest") *)

val empty : t
(** Empty forest for a free algebra *)

val find_dtree : int -> t -> algebra * (int * dtree) option
(** [find_dtree ar forest] returns a pair (arity,dtree) in given forest
    such that arity <= ar. Returns [None] when not found. *)

val pp_dtree : dtree printer
(** Printer for a single decision tree. *)

val pp_dforest : t printer
(** Printer for forests of decision trees. *)

val of_rules : name -> (name -> algebra) -> rule_infos list -> t
(** Compilation of rewrite rules into decision trees.
    Returns a list of arities and corresponding decision trees.
    Invariant : arities must be sorted in decreasing order.
    (see use case in [state_whnf] in [reduction.ml])
    May raise DtreeError.
*)
