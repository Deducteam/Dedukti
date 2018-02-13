(** Term reduction and conversion test. *)

open Term

type red = {
  select : (Rule.rule_name -> bool) option;
  beta : bool
}

type red_strategy = Hnf | Snf | Whnf | NSteps of int

val default : red

val select : red -> unit
(** [select filter] restrains the rules used during the reduction
    allowing only those whose name is accepted by the given [filter] function.
    [select None] is the default behaviour. *)

val reduction : Signature.t -> red_strategy -> term -> term
(** [hnf sg red te] reduces the term [te] according to the strategy [red]
    using the signature [sg]. *)

val are_convertible             : Signature.t -> term -> term -> bool
(** [are_convertible sg t1 t2] checks whether [t1] and [t2] are convertible
    in the signature [sg]. *)
