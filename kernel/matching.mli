(** Matching on terms *)

open Basic
open Term

exception NotUnifiable

(** [solve n k_lst te] solves following the higher-order unification problem (modulo beta):
    
    x{_1} => x{_2} => ... x{_[n]} => X x{_i{_1}} .. x{_i{_m}}
    {b =}
    x{_1} => x{_2} => ... x{_[n]} => [te]
    
    where X is the unknown, x{_i{_1}}, ..., x{_i{_m}} are distinct bound variables. *)
val solve : int -> int LList.t -> term -> term
(**
   If the free variables of [te] that are in x{_1}, ..., x{_[n]} are also in
   x{_i{_1}}, ..., x{_i{_m}} then the problem has a unique solution modulo beta that is
   x{_i{_1}} => .. => x{_i{_m}} => [te].
   Otherwise this problem has no solution and the function raises [NotUnifiable].
   
   Since we use deBruijn indexes, the problem is given as the equation
   
   x{_1} => ... => x{_[n]} => X DB(k{_0}) ... DB(k{_m}) =~ x{_1} => ... => x{_[n]} => [te]
   
   and where [k_lst] = [\[]k{_0}[; ]k{_1}[; ]...[; ]k{_m}[\]].
*)
