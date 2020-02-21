(** Term reduction and conversion test. *)
open Basic
open Term

val d_reduce : Debug.flag

type red_target   = Snf | Whnf
type red_strategy = ByName | ByValue | ByStrongValue

type red_cfg = {
  select   : (Rule.rule_name -> bool) option;
  nb_steps : int option; (* [Some 0] for no evaluation, [None] for no bound *)
  target   : red_target;
  strat    : red_strategy;
  beta     : bool;
  logger   : position -> Rule.rule_name -> term Lazy.t -> term Lazy.t -> unit
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

type convertibility_test = Signature.t -> term -> term -> bool
type matching_test = Rule.constr -> Rule.rule_name -> convertibility_test

exception Not_convertible

val eta : bool ref
(** Set to [true] to allow eta expansion at conversion check *)

module type ConvChecker = sig
  val are_convertible  : convertibility_test
  (** [are_convertible sg t1 t2] checks whether [t1] and [t2] are convertible
      or not in the signature [sg]. *)

  val matching_test : matching_test

  val conversion_step : Signature.t -> term * term -> (term * term) list -> (term * term) list
  (** [conversion_step sg (l,r) lst] returns a list [lst'] containing
      new convertibility obligations.
      Raise [NotConvertible] if the two terms are not convertible. *)
end

module type S = sig
  include ConvChecker

  val reduction : red_cfg -> Signature.t -> term -> term
  (** [reduction cfg sg te] reduces the term [te] following the configuration [cfg]
      and using the signature [sg]. *)

  val whnf : Signature.t -> term -> term
  (** [whnf sg t] returns the Weak Head Normal Form of [t].

      Definition: a term is in weak-head-normal form if there is a
      reduction strategy such that all its reducts following this strategy
      (including itself) have the same 'shape' at the root.

      The shape of a term could be computed like this:

      let rec shape = function
       | Type -> Type
       | Kind -> Kind
       | Pi _ -> Pi
       | Lam _ -> Lam
       | DB (_,_,n) -> DB n
       | Const (_,m,v) -> Const m v
       | App(f,a0,args) -> App (shape f,List.length (a0::args))

      Property:
      A (strongly normalizing) non weak-head-normal term can only have the form:
      - (x:A => b) a c_1..c_n, this is a beta-redex potentially with extra arguments.
      - or c a_1 .. a_n b_1 ..b_n with c a constant and c a'_1 .. a'_n is a gamma-redex
   where the (a'_i)s are reducts of (a_i)s. *)

  val snf  : Signature.t -> term -> term
  (** [sng sg t] returns the Strong Normal Form of [t].
      This may loop whenever [t] is not strongly normalizing. *)
end

module Make(C : ConvChecker) (M:Matching.Matcher) : S

module Default : S
