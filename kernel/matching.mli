(** Matching on terms *)

open Basic
open Term

exception NotUnifiable

(** ([n], [t]) represents the term represented by [t] under [n] lambda abstractions. *)
type 'a depthed = int * 'a

type dpos  = int depthed
type dterm = (term lazy_t) depthed

(* TODO: Should syntactic simply be Miller with the empty list ? *) 
type problem_type =
  | Syntactic (** a simple variable occurence  *)
  (** A Miller pattern problem [k_0 ; ... ; k_n] corresponds to
     the following matching problem (modulo beta):
        F( (DB k_0) ... (DB k_n)
     where F is the variable *)
  | MillerPattern of int LList.t

val mk_problem_type : int list -> problem_type

type 'a problem_relation =
  (** ... is exactly the provided depthed term (or position) *)
  | Eq        of 'a depthed
  (** ... is an AC symbol applied to elements of the [i]-th set ([n] occurences). *)
  | AC_Subset of int * int

(** ( ([l], [depth]), [m], [v], [n]) corresponds to the flattened set
 * of terms in [l] under [depth] lambda and headed by the [m].[v] AC symbol.
 * [n] variables (including Jokers) are subsets of this set. *)
type 'a ac_set  = 'a list depthed * ident * ident * int

type 'a problem =
  | Solved of 'a
  | Unsolved of (problem_type * 'a problem_relation) list

(** Infos to build the context from the stack *)
type 'a matching_problem =
  {
    (** For each variable a list of typed problems. *)
    problems    : 'a problem array;  (** Partial substituion. Initialized with Nones. *)
    ac_sets     : 'a ac_set array;  (** AC sets referred in AC subset problems. *)
  }

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
 * on depthed terms using:
 * - the [reduce] reduction strategy when necessary
 * - the [conv] convertability test
 *)
val solve_problem : (term -> term) ->
                    (term -> term -> bool) ->
                    (loc -> ident -> ident -> term list -> term) ->
                    term Lazy.t matching_problem -> term Lazy.t option array option

val pp_matching_problem : string -> (Format.formatter -> 'a -> unit) ->
                          Format.formatter -> 'a matching_problem -> unit

val pp_int_matching_problem : string -> Format.formatter -> int matching_problem -> unit
