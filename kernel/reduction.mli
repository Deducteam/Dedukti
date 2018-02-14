(** Term reduction and conversion test. *)

open Term

type red_strategy = Hnf | Snf | Whnf

type red_config = {
  select : (Rule.rule_name -> bool) option;
  nb_steps : int option; (* [Some 0] for no evaluation, [None] for no bound *)
  strategy : red_strategy;
  beta : bool
}

(** [beta] flag enables/disables beta reductions.
    [select] = [Some f] restreins rules according to the given filter on names.
    [select] = [None] is the default behaviour (all rules allowed). *)

val default : red_config

val reduction : red_config -> Signature.t -> term -> term
(** [reduction sg red te] reduces the term [te] following the strategy [red]
    and using the signature [sg]. *)

val are_convertible : Signature.t -> term -> term -> bool
(** [are_convertible sg t1 t2] check if [t1] and [t2] are convertible using the
    signature [sg]. *)
