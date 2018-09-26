(** Matching on terms *)

open Basic
open Term
open Ac

exception NotUnifiable

type Debug.flag += D_matching

(** ([n], [vars]) represents the [n]-th variable applied to the [vars] bound variables. *)
type var_p = int * int LList.t

(* TODO: add loc to this to better handle errors *)
type 'a problem =
  | Eq of var_p * 'a
  (** the variable is exactly the given term. *)
  | AC of ac_ident * int * (var_p list) * ('a list)
  (** [(cst, joks, u, vars, terms)]
   *  Represents the flattenned equality under AC([u]) symbol [cst] of:
   *  - [njoks] jokers and the given variables [vars]
   *  - The given [terms] *)

(** Problem with int referencing stack indices *)
type pre_matching_problem =
  {
    pm_problems : int problem depthed list; (** A list of problems under a certain depth. *)
    pm_miller   : int array                 (** A la Miller variables arity. *)
  }

(** int matching problem printing function (for dtree). *)
val pp_pre_matching_problem : string -> pre_matching_problem printer

type te = term Lazy.t

type status =
  | Unsolved                       (** X is unknown              *)
  | Solved of te                   (** X = [term]                *)
  | Partly of ac_ident * (te list) (** X = [m].[v](X', [terms])  *)

type matching_problem =
  {
    problems : te problem depthed list; (** A list of problems under a certain depth.       *)
    (* TODO: This array should somehow be immutable *)
    status   : status array;           (** Partial substituion. Initialized with Unsolved. *)
    (* TODO: This array should somehow be immutable *)
    miller   : int array               (** Variables Miller arity. *)
  }

val mk_matching_problem: (int -> te) -> (int list -> te list) ->
                         pre_matching_problem -> matching_problem


(** Generic matching problem printing function (for debug). *)
val pp_matching_problem : string -> matching_problem printer

(** solve_problem [reduce] [conv] [pb] solves the given matching problem
 * on lazy terms using:
 * - the [reduce] reduction strategy when necessary
 * - the [conv] convertability test
 *)
val solve_problem : (term -> term) ->
                    (term -> term -> bool) ->
                    (term -> term) ->
                    matching_problem -> te option array option
