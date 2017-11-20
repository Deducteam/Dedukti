(** Matching on terms *)

open Basic
open Term
open Ac

exception NotUnifiable

(** ([n], [t]) represents the term represented by [t] under [n] lambda abstractions. *)
type 'a depthed = int * 'a

(** ([n], [vars]) represents the [n]-th variable applied to the [vars] bound variables. *)
type var_p = int * int LList.t

(* TODO: add loc to this to better handle errors *)
type 'a problem =
  (** the variable is exactly the given term. *)
  | Eq of var_p * 'a
  (** ([m],[v],[joks],[u],[vars],[terms])
   *  Represents the flattenned equality under AC([u]) symbol [m].[v] of:
   *  - [njoks] jokers and the given variables [vars]
   *  - The given [terms] *)
  | AC of ac_ident * int * (var_p list) * ('a list)

type 'a status =
  | Unsolved                            (** X is unknown *)
  | Solved of 'a                        (** X = [term] *)
  | Partly of ac_ident * ('a list) (** X = [m].[v](X', [terms])  *)

(** Infos to build the context from the stack *)
type 'a matching_problem =
  {
    problems : 'a problem depthed list; (** A list of problems under a certain depth.       *)
    (* TODO: This array should be immutable *)
    status   : 'a status array;         (** Partial substituion. Initialized with Unsolved. *)
    (* TODO: This array should be immutable *)
    miller   : int array                (** Variables Miller arity. *)
  }

(** Translate matching problems from one type to the other using conversion functions. *)
val convert_problems : ('a -> 'b) -> ('a list -> 'b list) ->
                       'a matching_problem -> 'b matching_problem

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
                    term Lazy.t matching_problem -> term Lazy.t option array option

(** Generic matching problem printing function (for debug). *)
val pp_matching_problem : string -> 'a printer -> 'a matching_problem printer

(** int matching problem printing function (for dtree). *)
val pp_int_matching_problem : string -> int matching_problem printer
