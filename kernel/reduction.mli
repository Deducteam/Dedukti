(** Term reduction and conversion test. *)

open Term

type red = {
  select : (Rule.rule_name -> bool) option;
  beta : bool
}
(** [beta] flag enables/disables beta reductions.
    [select] = [Some f] restreins rules according to the given filter on names.
    [select] = [None] is the default behaviour (all rules allowed). *)

val default : red

val select : red -> unit
(** [select red] restreins reduction using the given filter. *)

type red_strategy = Hnf | Snf | Whnf

val reduction : red_strategy -> Signature.t -> term -> term
(** [reduction sg red te] reduces the term [te] following the strategy [red]
    and using the signature [sg]. *)

val reduction_steps : int -> red_strategy -> Signature.t -> term -> term
(** [reduction sg red n te] performs [n] reduction steps on the term [te]
    following the strategy [red] using the signature [sg]. *)

val are_convertible : Signature.t -> term -> term -> bool
(** [are_convertible sg t1 t2] check if [t1] and [t2] are convertible using the
    signature [sg]. *)
