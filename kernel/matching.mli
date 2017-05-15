(** Matching on terms *)

open Basic
open Term

exception NotUnifiable


(** solve [n] [k_lst] [te] solves the higher-order unification problems (unification modulo beta)
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
