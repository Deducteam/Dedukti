open Term
open Basic
open Rule
open Ac

(** {2 Error} *)

type rule_infos_error =
  | BoundVariableExpected          of loc * pattern
  | DistinctBoundVariablesExpected of loc * ident
  | VariableBoundOutsideTheGuard   of loc * term
  | UnboundVariable                of loc * ident * pattern
  | AVariableIsNotAPattern         of loc * ident
  | NonLinearNonEqArguments        of loc * ident
  | NotEnoughArguments             of loc * ident * int * int * int
  | NonLinearRule                  of loc * rule_name

type dtree_error =
  | HeadSymbolMismatch  of loc * name * name
  | ArityInnerMismatch  of loc * ident * ident
  | ACSymbolRewritten   of loc * name * int
  | RuleInfos            of rule_infos_error

exception Dtree_error of dtree_error

type miller_var = private
  {
    arity : int;
    (** Arity of the meta variable *)
    depth : int;
    (** Depth under which this occurence of the meta variable is considered *)
    vars : int list;
    (** The list of local DB indices of argument variables*)
    mapping : int array
    (** The mapping from all local DB indices for either -1 or position
        in the list of argument variables (starting from the end)
    *)
  }
(** This represent a meta variables applied to distinct
    locally bounded variables:  X x_1 ... x_n.
    - [arity] is the number of arguments
    - [depth] is the number of locally bounded variables available
    - [vars] is the list of successive arguments in order
    - [mapping] is a mapping for all available bounded variable n to
      - either -1 is this variable is absent from the list of arguments
      - or the index of that integer in the [vars] list

    The following invariants should therefore be verified:
    - [arity] is the length of vars
    - [depth] is the length of mapping
    - All elements of [vars] are between 0 and [depth]-1
    - Non negative elements of [mapping] are between 0 and [arity]-1
    - [mapping].(i) = n >= 0  iff  List.nth [vars] ([arity]-n-1) = i
    - This means exactly [arity] elements of [mapping] are non negative

    An example:
    \{
      arity   = 2;
      depth   = 5;
      vars    = [4; 2];
      mapping = [| (-1) ; (-1) ; 0 ; (-1) ; 1 |]
    \}
*)

val to_miller_var : int -> int -> int list -> miller_var
(** [to_miller_var depth arity vars] build a reverse mapping
    from the list [vars] of DB indices arguments of a Miller variable.
    For instance the pattern x => y => z => F y x produces a call to
    [mapping_of_vars 3 2 \[1; 0\] ] which returns the array
    [| 1 ; 0 ; (-1) |] *)

val fo_var : miller_var

(** {2 Pre-Matching problems} *)

(** Abstract matching problems. This can be instantiated with
    - When building a decision tree ['a = int] refers to positions in the stack
    - When matching against a term, ['a = term Lazy.t] refers to actual terms
*)

(* TODO: add loc to this to better handle errors *)
type 'a eq_problem = miller_var * 'a
(** [(vars, matched)] is the higher order equational problem:
       X x1  ... xn = [matched]   with [vars]=\[ x1 ; ... ; xn \] *)

type var_p = int * miller_var
(** ([n], [vars]) represents the [n]-th variable applied
    to the [vars] bound variables. *)

type 'a ac_problem = int * ac_ident * int * (var_p list) * 'a
  (** [(depth, symb, njoks, vars, terms)]
      Represents the flattenned equality under AC symbol [symb] of:
      - [njoks] jokers and the given variables [vars]
      - The given [terms]
      e.g.
        [ +{ X\[x\] , _, Y\[y,z\] } = +{ f(a), f(y), f(x)} ]
      the [depth] field in all elements of [vars] should be equal to [depth]
      FIXME: do we need [depth] here then ?
   *)

type pre_matching_problem =
  {
    pm_eq_problems : int eq_problem list LList.t;
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


(** {2 Rule_infos} *)

(** Efficient representation for well-formed linear Miller pattern *)
type wf_pattern = private
  | LJoker
  | LVar      of ident * int * int list         (** Applied Miller variable *)
  | LLambda   of ident * wf_pattern             (** Lambda abstraction      *)
  | LPattern  of name * wf_pattern array        (** Applied constant        *)
  | LBoundVar of ident * int * wf_pattern array (** Locally bound variable  *)
  | LACSet    of name * wf_pattern list


val pp_wf_pattern      : wf_pattern      printer

(** [constr] is the type of brackets (aka "dot") pattern constraints.
    They are generated by the function check_patterns.
    [(i,te)] means meta variable of DB index [i] should be convertible with [te] *)
type constr = int * term

val pp_constr : constr printer


type rule_infos  = private
  {
    l           : loc;
    (** location of the rule *)
    name        : rule_name;
    (** name of the rule *)
    nonlinear   : int list;
    (** DB indices of non linear variables. Empty if the rule is linear ? *)
    cst         : name;
    (** name of the pattern constant *)
    args        : pattern list;
    (** arguments list of the pattern constant *)
    rhs         : term;
    (** right hand side of the rule *)
    ctx_size    : int;
    (** size of the context of the non-linear version of the rule *)
    esize       : int;
    (** size of the context of the linearized, bracket free version of the rule *)
    pats        : wf_pattern array;
    (** free patterns without constraint *)
    arity       : int array;
    (** arities of context variables *)
    constraints : constr list;
    (** constraints generated from the pattern to the free pattern *)
    ctx         : term option context;
    (** context given by the user *)
  }

val pattern_of_rule_infos : rule_infos -> pattern
(** Extracts LHS pattern from a rule info *)

val to_rule_infos : untyped_rule -> rule_infos
(** Converts any rule (typed or untyped) to rule_infos *)

val untyped_rule_of_rule_infos : rule_infos -> untyped_rule
(** Converts rule_infos representation to a rule where
    the context is annotated with the variables' arity *)

val arities_of_rule : rule_infos -> arity_context
(** Rule where context is annotated with variable arities *)

val pp_rule_infos      : rule_infos      printer

val check_arity : rule_infos -> unit

val check_linearity : rule_infos -> unit

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
    May raise Dtree_error.
*)
