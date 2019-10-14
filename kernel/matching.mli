(** Matching on terms *)

open Basic
open Term
open Ac
open Dtree


val d_matching : Debug.flag

(** {2 Matching problems} *)

type te = term Lazy.t

type status =
  | Unsolved                       (** X is unknown              *)
  | Solved of te                   (** X = [term]                *)
  | Partly of ac_ident * (te list) (** X = [m].[v](X', [terms])  *)

type matching_problem =
  {
    eq_problems : te eq_problem list array;
    (** A list of equational problems under various depths for
        each variable *)
    ac_problems : te list ac_problem list;
    (** A list of AC problems under a certain depth *)
    status      : status array;
    (** Partial substitution. Initialized with Unsolved *)
    arity       : int array  (* TODO: is array should be immutable *)
    (** Variables Miller arity. *)
  }

val mk_matching_problem: (int -> te) -> (int -> te list) ->
                         pre_matching_problem -> matching_problem

(** Generic matching problem printing function (for debug). *)
val pp_matching_problem : string -> matching_problem printer


(** {2 Matching solver} *)

module type Checker = sig
  val snf  : Signature.t -> term -> term
  val whnf : Signature.t -> term -> term
  val are_convertible : Signature.t -> term -> term -> bool
end

module type Matcher = sig
  val solve_problem : Signature.t -> matching_problem -> te array option
  (** [solve_problem [reduce] [conv] [pb] solves the given matching problem
   * on lazy terms using:
   * - the [reduce] reduction strategy when necessary
   * - the [conv] convertability test
  *)

  (** [solve n k_lst te] solves following the higher-order unification problem (modulo beta):

    x{_1} => x{_2} => ... x{_[n]} => X x{_i{_1}} .. x{_i{_m}}
    {b =}
    x{_1} => x{_2} => ... x{_[n]} => [te]

    where X is the unknown, x{_i{_1}}, ..., x{_i{_m}} are distinct bound variables. *)

  (**
   If the free variables of [te] that are in x{_1}, ..., x{_[n]} are also in
   x{_i{_1}}, ..., x{_i{_m}} then the problem has a unique solution modulo beta that is
   x{_i{_1}} => .. => x{_i{_m}} => [te].
   Otherwise this problem has no solution and the function raises [NotUnifiable].

   Since we use deBruijn indexes, the problem is given as the equation

   x{_1} => ... => x{_[n]} => X DB(k{_0}) ... DB(k{_m}) =~ x{_1} => ... => x{_[n]} => [te]

   and where [k_lst] = [\[]k{_0}[; ]k{_1}[; ]...[; ]k{_m}[\]].
*)
end

module Make (C:Checker) : Matcher
