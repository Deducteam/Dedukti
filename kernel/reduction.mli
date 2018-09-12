(** Term reduction and conversion test. *)
open Basic
open Term

type Debug.flag += D_reduce

type red_target   = Snf | Whnf
type red_strategy = ByName | ByValue | ByStrongValue

type red_cfg = {
  select   : (Rule.rule_name -> bool) option;
  nb_steps : int option; (* [Some 0] for no evaluation, [None] for no bound *)
  target   : red_target;
  strat    : red_strategy;
  beta     : bool;
  logger   : position -> Rule.rule_name -> term Lazy.t -> unit
}
(** Configuration for reduction.
    [select] = [Some f] restreins rules according to the given filter on names.
    [select] = [None] is the default behaviour (all rules allowed).
    [nb_steps] = [Some n] Allows only [n] reduction steps.
    [nb_steps] = [None] is the default behaviour.
    [target] is the normal form to compute.
    [strat] is the reduction strategy.
    [beta] flag enables/disables beta reductions.
    [logger] is the function to call upon applying a reduction rule.
*)

val pp_red_cfg : red_cfg printer

val default_cfg : red_cfg
(** default configuration where:
    - [select]   = [None]
    - [nb_steps] = [None]
    - [strategy] = [ByName]
    - [target]   = [Snf]
    - [beta]     = [true]
    - [logger]   = [fun _ _ _ -> ()]
*)

val reduction : red_cfg -> Signature.t -> term -> term
(** [reduction cfg sg te] reduces the term [te] following the configuration [cfg]
    and using the signature [sg]. *)

val default_reduction : red_target -> Signature.t -> term -> term
(** [default_reduction tar sg te] reduces the term [te] to its [tar] normal form
    using the signature [sg]. This is the fastest implementation used for typing. *)

exception NotConvertible

val conversion_step : term * term -> (term * term) list -> (term * term) list
(** [conversion_step (l,r) lst] returns a list [lst'] containing new convertibility obligations.
    Raise [NotConvertible] if the two terms cannot be convertible. *)

val are_convertible : Signature.t -> term -> term -> bool
(** [are_convertible sg t1 t2] checks whether [t1] and [t2] are convertible
    or not in the signature [sg]. *)

module type RE = sig

  val whnf            : Signature.t -> term -> term
  val snf             : Signature.t -> term -> term
  val are_convertible : Signature.t -> term -> term -> bool
end

module REDefault : RE
