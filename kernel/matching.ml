open Basic
open Term

exception NotUnifiable

(** Solve the following problem for lambda term X:
 *    (lambda^[depth]. X) [args] = [t] where [args] are distincts bound variables
 * Raises NotUnifiable if [t] contains variables not in [args]. *)
let solve_miller (depth:int) (args:int LList.t) (te:term) : term =
  let size = LList.len args in
  let arr = Array.make depth None in
  List.iteri ( fun i n -> arr.(n) <- Some (size-i-1) ) (LList.lst args);
  (* TODO: This could be computed once for all at compile time *)
  let rec aux k = function
    | Type _ | Kind | Const _ as t -> t
    | DB (l,x,n) as t ->
      if n < k             (* var bound in te *) then t
      else if n >= k+depth (* var free  in te *) then mk_DB l x (n-depth+size)
      else mk_DB l x (match arr.(n-k) with None -> raise NotUnifiable | Some n' -> n'+k)
    | Lam (l,x,a,b) -> mk_Lam l x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (l,x,a,b) -> mk_Pi  l x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in
  aux 0 te

let rec add_n_lambdas n t =
  if n == 0 then t else add_n_lambdas (n-1) (mk_Lam dloc dmark None t)

(** [solve n k_lst te] solves the following higher-order unification problem:
    (unification modulo beta)

    x{_1} => x{_2} => ... x{_[n]} => X x{_i{_1}} .. x{_i{_m}}
    {b =}
    x{_1} => x{_2} => ... x{_[n]} => [te]

    where X is the unknown, x{_i{_1}}, ..., x{_i{_m}} are distinct bound variables
    in the local context and [te] is a term.

   If the free variables of [te] that are in x{_1}, ..., x{_[n]} are also in
   x{_i{_1}}, ..., x{_i{_m}} then the problem has a unique solution modulo beta that is
   x{_i{_1}} => .. => x{_i{_m}} => [te].
   Otherwise this problem has no solution and the function raises [NotUnifiable].

   Since we use deBruijn indexes, the problem is given as the equation

   x{_1} => ... => x{_[n]} => X DB(k{_0}) ... DB(k{_m}) =~ x{_1} => ... => x{_[n]} => [te]

   and where [k_lst] = [\[]k{_0}[; ]k{_1}[; ]...[; ]k{_m}[\]].
*)
let solve (depth:int) (args:int LList.t) (te:term) : term =
  if LList.is_empty args
  then try Subst.unshift depth te with Subst.UnshiftExn -> raise NotUnifiable
  else add_n_lambdas (LList.len args) (solve_miller depth args te)
