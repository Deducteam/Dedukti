open Basic
open Term
open Rule

type index = int

val pp_index : index printer

type symb_status = Set_constructor | Elt_constructor | Def_function | Def_type

val pp_status : symb_status printer

type local_result = SelfLooping of (index list) | CocOption
                  | UsingBrackets | NonPositive | NotHandledRewritingTypeLevel

val find_stat : name -> symb_status

type symbol = {
    ind            : index             ; (** The index of the symbol *)
    arity          : int               ; (** Arity of the symbol (number of args). *)
    typ            : term              ; (** Type of the symbol *)
    mutable status : symb_status       ; (** The status of the symbol *)
    mutable result : local_result list ; (** The information about non termination for this symbol, initially empty *)
  }

val update_result : name -> local_result -> unit

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
end

module IMap : sig
  type key = index
  type +'a t
  val find : key -> 'a t -> 'a
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
end

type call =
  { callee      : name      ; (** Key of the function symbol being called. *)
    caller      : name      ; (** Key of the calling function symbol. *)
    matrix      : Sizematrix.matrix     ; (** Size change matrix of the call. *)
    rules       : index list ; (** The list of rules leading to this call *)
  }

val pp_call : call printer

type call_graph ={
    next_index      : index ref             ; (** The index of the next function symbol to be added *)
    next_rule_index : index ref             ; (** Same here *)
    symbols         : symbol NMap.t ref     ; (** A map containing every symbols studied *)
    all_rules       : rule_infos IMap.t ref ; (** A map containing every rules, in order to trace the succession of rule leading to non-termination *)
    calls           : call list ref         ; (** The list of every call *)
  }

val graph : call_graph ref

val rule_to_call : int -> rule_infos -> call list
    
val add_call : call-> unit

val infer_arity_from_type : term -> int
