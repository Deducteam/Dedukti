(** Matching on terms *)

open Basic
open Term
open Ac

(** ([n], [t]) represents the term represented by [t] under [n] lambda abstractions. *)
type 'a depthed = int * 'a

(** ([n], [vars]) represents the [n]-th variable applied to the [vars] bound variables. *)
type var_p = int * int LList.t

(* TODO: add loc to this to better handle errors *)
type 'a problem =
  | Eq of var_p * 'a
  (** the variable is exactly the given term. *)
  | AC of ac_ident * int * (var_p list) * ('a list)
  (** ([cst],[joks],[u],[vars],[terms])
   *  Represents the flattenned equality under AC([u]) symbol [cst] of:
   *  - [njoks] jokers and the given variables [vars]
   *  - The given [terms] *)

(** Problem with int referencing stack indices *)
type pre_matching_problem =
  {
    pm_problems : int problem depthed list; (** A list of problems under a certain depth. *)
    pm_miller   : int array                 (** Variables Miller arity. *)
  }

type te = term Lazy.t

type status =
  | Unsolved                      (** X is unknown *)
  | Solved of te                   (** X = [term] *)
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


(** solve [n] [k_lst] [te] solves the higher-order unification problems
    (unification modulo beta)
      x_1 => x_2 ... x_n => X x_(i_1) .. x_(i_m) = x_1 => x_2 ... x_n => te
      where X is the unknown, x_(i_1) .. x_(i_m) are distinct (bound) variables and [te] is a term.*)
val solve : int -> int LList.t -> term -> term
(**
  If the free variables of [te] that are in x_1 .. x_n are also in x_(i_1) .. x_(i_m) then
  the problem has a unique solution (modulo beta) that is
  x_(i_1) => .. => x_(i_m) => te.
  Otherwise this problem has no solution.

  Since we use deBruijn indexes, the problem is given as the equation
  x_1 => .. => x_n => X (DB [k_0]) ... (DB [k_n]) =~ x_1 => .. => x_n => [te]
  and where [k_lst] = \[[k_0::k_1::..::k_m]\].
 *)


(** solve_problem [reduce] [conv] [pb] solves the given matching problem
 * on lazy terms using:
 * - the [reduce] reduction strategy when necessary
 * - the [conv] convertability test
 *)
val solve_problem : (term -> term) ->
                    (term -> term -> bool) ->
                    matching_problem -> te option array option

(** Generic matching problem printing function (for debug). *)
val pp_matching_problem : string -> matching_problem printer

(** int matching problem printing function (for dtree). *)
val pp_pre_matching_problem : string -> pre_matching_problem printer
