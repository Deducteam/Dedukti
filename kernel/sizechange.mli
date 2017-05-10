open Basic
open Term
open Rule
open Format

val add_fonc : bool -> ident -> Term.term -> unit
       
val add_rules : bool -> untyped_rule list -> unit

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

(** Special index denoting the entry point. *)
val root : index

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
  ; matrix : matrix (** Size change matrix of the call. *)
  ; is_rec : bool   (** Indicates if this is a recursive call. *) }

(** The representation of the call graph. *)
type call_graph

val initialize : call_graph ref
val table : (string*int) list ref
val nom_module : ident -> string ref
val finalize : bool
  
(** [create ()] returns a new, empty call graph. *)
val create : unit -> call_graph ref

(** [copy g] returns a copy of the call graph [g]. *)
val copy : call_graph -> call_graph

(** [is_empty g] indicates whether the call graph [g] contains calls. *)
val is_empty : call_graph -> bool



(** [latex_print_calls ff g] prints the call graph [g] using a LaTeX format
    on the [Format.formatter] [ff]. *)
val latex_print_calls : unit -> unit
