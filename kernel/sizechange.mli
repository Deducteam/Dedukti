open Basic
open Term
open Rule
open Format

exception Calling_unknown of int
exception NonLinearity of int
exception TypingError of name
exception NonPositivity of name
exception ModuleDependancy of name
exception PatternMatching of int
exception TypeLevelRewriteRule of (name * name)
exception TypeLevelWeird of (name * term)
exception ProductIncompatibility
exception BracketPatternMatching of int
exception OverApplication

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
    [b]-th argument of the callee. The boolean [is_rec] is true when the
    call is a reccursive call (i.e. a call to a generalised hypothesis
    lower in the tree. It is [false] for every call to subtyping in the
    typing algorithm and the same goes for rules introducing a new
    induction hypothesis. Every other call refers to a previously
    introduced induction hypothesis and its boolean is [true]. *)
type call =
  { callee : index  (** Key of the function symbol being called. *)
  ; caller : index  (** Key of the calling function symbol. *)
  ; matrix : matrix (** Size change matrix of the call. *)}

(** The representation of the call graph. *)
type call_graph

(** [latex_print_calls ff g] prints the call graph [g] using a LaTeX format
    on the [Format.formatter] [ff]. *)
val latex_print_calls : unit -> unit

val termination_check : bool -> bool -> mident -> rule_infos list list ->
  (name * Signature.staticity * term *
     (rule_infos list*int*Dtree.dtree) option
  ) list -> bool
