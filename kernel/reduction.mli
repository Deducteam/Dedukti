(** Term reduction and conversion test. *)

open Basic
open Term


module type Reducer =
sig
  val whnf : Signature.t -> term -> term (** Weak head normal form. *)
  val snf  : Signature.t -> term -> term (** Strong normal form. *)
  val hnf  : Signature.t -> term -> term (** Head normal form. *)
  val are_convertible : Signature.t -> term -> term -> bool
  (** [are_convertible sg t1 t2] checks whether [t1] and [t2] are convertible
      or not in the signature [sg]. *)
end

module StdReducer : Reducer (** Standard reducer *)


type red_strategy = Hnf | Snf | Whnf

type red_cfg = {
  select : (Rule.rule_name -> bool) option;
  nb_steps : int option; (* [Some 0] for no evaluation, [None] for no bound *)
  strategy : red_strategy;
  beta : bool
}

val pp_red_cfg : red_cfg printer

(** [beta] flag enables/disables beta reductions.
    [select] = [Some f] restreins rules according to the given filter on names.
    [select] = [None] is the default behaviour (all rules allowed). *)

val default_cfg : red_cfg
(** default configuration where:
    [select] = [None] ;
    [nb_steps] = [None] ;
    [strategy] = [Snf] ;
    [beta] = [true] ;
*)


(** Parameterized reducer *)
module ParamReducer : sig
  include Reducer
  val set_strategy : red_cfg -> unit
  val reset : unit -> unit
end

