open Basic
open Term
open Rule
open Format

(** Representation of the set {-1, 0, ∞} *)
type cmp = Min1 | Zero | Infi

(** [cmp_to_string c] returns a string representation of the given [cmp]
    element on one character. *)
val cmp_to_string : cmp -> string

(** Size change matrix. *)
type matrix = { w : int ; h : int ; tab : cmp array array }

(** Abstract type used to refer to function symbols. *)
type index

(** [int_of_index i] returns an [int] corresponding to the index [i]. *)
val int_of_index : index -> int

(** A call [{callee; caller; matrix; is_rec}] represents a call to the
    function symbol with key [callee] by the function symbole with the
    key [caller]. The [matrix] gives the relation between the parameters
    of the caller and the callee. The coefficient [matrix.(a).(b)] give
    the relation between the [a]-th parameter of the caller and the
    [b]-th argument of the callee. *)
type call =
  { callee : index           ; (** Key of the function symbol being called. *)
    caller : index           ; (** Key of the calling function symbol. *)
    matrix : matrix          ; (** Size change matrix of the call. *)
    rules  : index list        (** The list of rules leading to this call *)
  }

type call_graph

val termination_check : mident -> rule_infos list list ->
  (name * Signature.staticity * term *
     (rule_infos list*Dtree.t) option
  ) list -> bool

type global_result=Terminating | G_SelfLooping
                  | G_UsingBrackets | G_NonPositive | G_CriticalPair
                  | G_NotHandledRewritingTypeLevel

val table_result : (global_result, Basic.name list) Hashtbl.t
val list_SelfLooping : (Basic.name * index list) list ref

val pp_list_of_self_looping_rules : (Basic.name * index list) printer
