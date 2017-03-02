open Basic
open Rule

type dtree_error =
  | NotEnoughArguments of loc * ident * int * int * int
  | HeadSymbolMismatch of loc * ident*ident
  | ArityMismatch of loc * ident
  | UnboundVariable of loc * ident * pattern
  | AVariableIsNotAPattern of loc * ident
  | NonLinearRule of typed_rule


(** {2 Decision Trees} *)

(** There are three cases to match arguments of a pattern. Either the argument is
    - a constant
    - a variable
    - a lambda expression
*)
type case =
  | CConst of int*ident*ident
  (** [size] [m] [v] where size is the number of *static* arguments expected for the constant m.v *)
  | CDB    of int*int
  (** [size] [i] where size is the number of *static* arguments expected for the variable (as a function) [i] *)
  | CLam
  (** Just a lambda term *)

(* Abstract (from a stack (or a term list)) matching problem *)
type abstract_pb = { position2:int (*c*) ; dbs:int LList.t (*(k_i)_{i<=n}*) ; depth2:int }
(* It corresponds to the following matching problem (modulo beta):
 * stck.(c) ~? F( (DB k_0) ... (DB k_n) )
 * where F is the variable
 * *)

type pos = { position:int; depth:int }

(* FIXME: change the name of this type *)
(* Infos to build the context from the stack *)
type pre_context =
  | Syntactic of pos LList.t
  (* the list of positions in the stack corresponding to the context. *)
  | MillerPattern of abstract_pb LList.t
  (* the list of abstract problem which list of solutions gives the context. *)

val pp_pre_context : Format.formatter -> pre_context -> unit

type dtree =
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of pre_context * constr list * Term.term * dtree option

val pp_dtree : Format.formatter -> dtree -> unit

type rw = ident * ident * int * dtree

val pp_rw : Format.formatter -> rw -> unit


val to_rule_infos : typed_rule -> (rule_infos, dtree_error) error

val of_rules : rule_infos list -> (int * dtree, dtree_error) error
(** Compilation of rewrite rules into decision trees. *)
