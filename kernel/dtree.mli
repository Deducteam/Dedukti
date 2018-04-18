open Term
open Basic
open Rule
open Matching

(** {2 Error} *)

type dtree_error =
  | HeadSymbolMismatch  of loc * name * name
  | ArityInnerMismatch  of loc * ident * ident
  | ArityDBMismatch     of loc * name * int
  | AritySymbolMismatch of loc * name * name
  | ACSymbolRewritten   of loc * name * int

(** {2 Decision Trees} *)

(** Arguments of a pattern may be the following:
    - a constant
    - a variable
    - a lambda expression *)
type case =
  | CConst of int * name * bool
  (** [size c ac] where [size] is the number of arguments expected for the
      constant [c] and [ac] is true iff the constant is a definable AC(U) symbol. *)
  | CDB of int * int
  (** [size i] where size is the number of *static* arguments expected for the
      bounded variable [i] *)
  | CLam (** A lambda term *)


(** Represent the position of an argument in a pattern *)
type arg_pos =
  {
    position:int; (** position of the argument from left to right of the pattern *)
    depth:int (** depth of the argument regarding absractions *)
  }

(** An abstract problem [arg, \[k_0 ; ... ; k_n \]] corresponds to the following matching problem (modulo beta):
     stck.(arg.position) ~? F( (DB k_0) ... (DB k_n)
     where F is the variable *)
type abstract_problem = arg_pos * int LList.t

(** Infos to build the context from the stack *)
type matching_problem =
  (* FIXME: MillerPattern is stricly more general than Syntactic, are we loosing efficency by removing the Syntactic constructor ? *)
  | Syntactic of arg_pos LList.t
  (** the list of positions in the stack corresponding to the context. *)
  | MillerPattern of abstract_problem LList.t
  (** the list of abstract problem which list of solutions gives the context. *)

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
(** Empty forest *)

val find_dtree : int -> t -> (int * dtree) option
(** [find_dtree ar forest] returns a pair (arity,dtree) in given forest
    such that arity <= ar. Returns [None] when not found. *)

val pp_dtree : dtree printer
(** Printer for a single decision tree. *)

val pp_dforest : t printer
(** Printer for forests of decision trees. *)


val of_rules : (name -> algebra) -> rule_infos list -> (t, dtree_error) error
(** Compilation of rewrite rules into decision trees.
Returns a list of arities and corresponding decision trees.
Invariant : arities must be sorted in decreasing order.
(see use case in [state_whnf] in [reduction.ml])
*)
