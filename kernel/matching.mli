(** Matching on terms *)
open Basic
open Term
open Dtree

val d_matching : Debug.flag

(** {2 Matching solver} *)

exception NotUnifiable

val solve_miller : miller_var -> term -> term
(** [solve_miller var t]
    Solves the matching problem X x1 ... xn = [t]
    where [var] represents the applied Miller variable X x1 ... xn.
    Raises NotUnifiable in case there is no solution.
*)

module type Reducer = sig
  val snf  : Signature.t -> term -> term
  val whnf : Signature.t -> term -> term
  val are_convertible : Signature.t -> term -> term -> bool
end

module type Matcher = sig
  val solve_problem :
    Signature.t -> (int -> term Lazy.t) -> (int -> term Lazy.t list) ->
    pre_matching_problem -> term Lazy.t LList.t option
  (** [solve_problem sg eq_conv ac_conv pb] solves the [pb] matching problem
      using the given functions to convert positions in the stack to actual
      (lazy) terms.
  *)
end

module Make (R:Reducer) : Matcher
(** This is the default implementation.
 * It relies on the provided :
 * - [whnf] reduction strategy to flatten AC terms without digging to deep inside
 * - [snf] reduction strategy to perform higher order matching when necessary
 * - [are_convertible] convertibility test
*)
