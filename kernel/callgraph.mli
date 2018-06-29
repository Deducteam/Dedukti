open Basic
open Term
open Rule

(** Index of a rule *)
type index = int

(** The pretty printer for the type [index] *)
val pp_index : index printer

(** The status expresses if a symbol is a constructor or not *)
type symb_status = Set_constructor | Elt_constructor | Def_function | Def_type

(** The pretty printer for the type [symb_status] *)
val pp_status : symb_status printer

(** The local result express the result of the termination checker for this symbol *)
type local_result = SelfLooping of (index list) | CocOption
                  | UsingBrackets | NonPositive | NotHandledRewritingTypeLevel

(** Representation of a function symbol. *)
type symbol = {
    ind            : index             ; (** The index of the symbol *)
    arity          : int               ; (** Arity of the symbol (number of args). *)
    typ            : term              ; (** Type of the symbol *)
    mutable status : symb_status       ; (** The status of the symbol *)
    mutable result : local_result list ; (** The information about non termination for this symbol, initially empty *)
  }

(** Map with [index] as keys. *)
module IMap : sig
  type key = index
  type +'a t
  val find : key -> 'a t -> 'a
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
end

(** Map with [Basic.name] as keys *)
module NMap : sig
  type key = name
  type +'a t
  val find : key -> 'a t -> 'a
  val cardinal : 'a t -> int
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val mem : key -> 'a t -> bool
end

(** A call [{callee; caller; matrix; is_rec}] represents a call to the function symbol with key [callee] by the function symbole with the key [caller].
    The [matrix] gives the relation between the parameters of the caller and the callee.
    The coefficient [matrix.tab.(a).(b)] give the relation between the [a]-th parameter of the caller and the [b]-th argument of the callee.
    [rules] is the list of indexes of rules which lead to this call-matrix in the graph. *)
type call =
  { callee      : name      ; (** Key of the function symbol being called. *)
    caller      : name      ; (** Key of the calling function symbol. *)
    matrix      : Sizematrix.matrix     ; (** Size change matrix of the call. *)
    rules       : index list ; (** The list of rules leading to this call *)
  }

(** The pretty printer for the type [call]. *)
val pp_call : call printer
    
(** Internal state of the SCT, including the representation of symbols and the call graph. *)
type call_graph ={
    next_index      : index ref             ; (** The index of the next function symbol to be added *)
    next_rule_index : index ref             ; (** Same here *)
    symbols         : symbol NMap.t ref     ; (** A map containing every symbols studied *)
    all_rules       : rule_infos IMap.t ref ; (** A map containing every rules, in order to trace the succession of rule leading to non-termination *)
    calls           : call list ref         ; (** The list of every call *)
  }

(** The call graph which will be studied *)
val graph : call_graph ref

(** [find_stat f] will return the status [s] of the symbol named [f] as stated in the call graph. *)
val find_stat : name -> symb_status

(** Those functions modify the mutable fields in the symbol records *)
val update_result : name -> local_result -> unit
  
(** Generate the list of calls associated to a rule. An int is used to specified under how many lambdas are the call *)
val rule_to_call : int -> rule_infos -> call list

(** Add a new call to the call graph. *)
val add_call : call-> unit

val infer_arity_from_type : term -> int
