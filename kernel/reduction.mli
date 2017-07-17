(** Term reduction and conversion test. *)

open Term

type red = {
  select : (Rule.rule_name -> bool) option;
  beta : bool
}

type red_strategy = Hnf | Snf | Whnf | OneStep

val default : red

val select      : red -> unit
(** [select [Some [md1,...,mdn]]] restreins rules used during the reduction. Only rules declared in signature mdi are allowed. [select None] is the default behaviour. *)

val reduction : Signature.t -> red_strategy -> term -> term
(** [hnf sg red te] Reduce the term [te] according to the strategy [red] using the signature [sg]. *)

val are_convertible             : Signature.t -> term -> term -> bool
(** [are_convertible sg t1 t2] check if [t1] and [t2] are convertible using the signature [sg]. *)
