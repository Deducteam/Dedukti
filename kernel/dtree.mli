open Basic
open Rule

(** {2 Decision Trees} *)

(** Arguments of a pattern may be the following:
    - a constant
    - a variable
    - a lambda expression *)
type case =
  | CConst of int*ident*ident
  (** [size] [m] [v] where [size] is the number of *static* arguments expected for the constant [m.v] *)
  | CDB    of int*int
  (** [size] [i] where size is the number of *static* arguments expected for the bounded variable [i] *)
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

val pp_matching_problem : Format.formatter -> matching_problem -> unit

(** Type of decision trees *)
type dtree =
  | Switch  of int * (case*dtree) list * dtree option (** Switch [i] [(case_0,tree_0) ; ... ; (case_n, tree_n)] [tree_opt] test if the [i] arg of a pattern can be match with one of the case of the list. if it does then look at the corresponding tree, otherwise, look at the default tree *)
  | Test    of matching_problem * constr list * Term.term * dtree option (** Test [pb] [cstrs] [te] [tree_opt] are the leaves of the tree. Check that each problem can be solves and such that constraints are satisfied. If it does then return a local context for the term [te]. *)

val pp_dtree : Format.formatter -> dtree -> unit

(** [md] [v] [i] [tree] is the dtree associated to the constant [md].[v] with [i] arguments *)
type rw = ident * ident * int * dtree

val pp_rw : Format.formatter -> rw -> unit

(** {2 Error} *)

type dtree_error =
  | HeadSymbolMismatch of loc * ident * ident
  | ArityMismatch of loc * ident

(** Compilation of rewrite rules into decision trees. *)
val of_rules : rule_infos list -> (int * dtree, dtree_error) error
