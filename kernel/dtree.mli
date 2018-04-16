open Basic
open Rule

(** {2 Error} *)

type dtree_error =
  | HeadSymbolMismatch of loc * name * name
  | ArityInnerMismatch of loc * ident * ident

(** {2 Decision Trees} *)

(** Arguments of a pattern may be the following:
    - a constant
    - a variable
    - a lambda expression *)
type case =
  | CConst of int * name
  (** [size c] where [size] is the number of *static* arguments expected for the constant [c] *)
  | CDB of int * int
  (** [size i] where size is the number of *static* arguments expected for the bounded variable [i] *)
  | CLam (** Just a lambda term *)
(** Since the arity of a constant can not be know statically, size should be always smaller than the number of arguments applied to the constant m.v *)


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
  | Test of rule_name * matching_problem * constr list * Term.term * dtree option
  (** [Test name pb cstrs rhs default_tree] are the leaves of the tree.
      Checks that each problem can be solved such that constraints are satisfied.
      If it does then return a local context for the term [rhs]. *)

(** Type of decision forest *)
type t

val empty : t

val find_dtree : int -> t -> (int * dtree) option
(** [find_dtree ar forest] returns a pair (arity,dtree) in given forest
    such that arity <= ar. Returns [None] when not found. *)


val pp_matching_problem : matching_problem printer

(** Printer for a single decision tree. *)
val pp_dtree : dtree printer

(** Printer for forests of decision trees. *)
val pp_dforest : t printer


(** Compilation of rewrite rules into decision trees.
Returns a list of arities and corresponding decision trees.
Invariant : arities must be sorted in decreasing order.
(see use case in [state_whnf] in [reduction.ml])
*)
val of_rules : rule_infos list -> (t, dtree_error) error
