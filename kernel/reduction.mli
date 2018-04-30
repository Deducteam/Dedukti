(** Term reduction and conversion test. *)

open Basic
open Term

type red_strategy = Hnf | Snf | Whnf

type red_cfg = {
  select   : (Rule.rule_name -> bool) option;
  nb_steps : int option; (* [Some 0] for no evaluation, [None] for no bound *)
  strategy : red_strategy;
  beta     : bool
}

val pp_red_cfg : red_cfg printer

type step = Rule.rule_name option (* None for Beta *)

type trace =
  {
    left  : step list;
    right : step list
  }

val get_trace : unit -> trace

val pp_trace : trace printer

(** [beta] flag enables/disables beta reductions.
    [select] = [Some f] restreins rules according to the given filter on names.
    [select] = [None] is the default behaviour (all rules allowed). *)

val default_cfg : red_cfg
(** default configuration where:
    [select]   = [None] ;
    [nb_steps] = [None] ;
    [strategy] = [Snf]  ;
    [beta]     = [true] ;
    [trace]    = [false ;
*)

val reduction : red_cfg -> Signature.t -> term -> term
(** [reduction sg red te] reduces the term [te] following the strategy [red]
    and using the signature [sg]. *)

val are_convertible : Signature.t -> term -> term -> bool
(** [are_convertible sg t1 t2] checks whether [t1] and [t2] are convertible
    or not in the signature [sg]. *)
