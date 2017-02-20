open Basic
open Term

exception NotUnifiable

let update_dbs (depth:int) (dbs:int LList.t) (te:term) : term =
  let size = LList.len dbs in
  let arr = Array.make size None in
  let _ = List.iteri ( fun i n -> arr.(n) <- Some (size-i-1) ) (LList.lst dbs) in (* XXX: could be computed once for all at compile time *)
  let rec aux k = function
    | Type _ | Kind | Const _ as t -> t
    | DB (l,x,n) as t ->
        if n < k (* var bound in te *) then t
        else if n >= k+depth (* var free in te*) then
          mk_DB l x (n+size)
        else
          ( match arr.(n-k) with
            | None -> raise NotUnifiable
            | Some n' -> mk_DB dloc x (n'+k) )
    | Lam (l,x,a,b) -> mk_Lam dloc x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi  dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in
  aux 0 te

(* This function solves higher-order unification problems (unification modulo beta)
 * of the form:
 * x_1 => x_2 ... x_n => X x_(i_1) .. x_(i_m) = x_1 => x_2 ... x_n => te
 * where X is the unknown, x_(i_1) .. x_(i_m) are distinct (bound) variables and te is a term.
 *
 * If the free variables of te that are in x_1 .. x_n are also in x_(i_1) .. x_(i_m) then
 * the problem has a unique solution (modulo beta) that is
 * x_(i_1) => .. => x_(i_m) => te.
 * Otherwise this problem has no solution.
 *
 * Since we use deBruijn indexes, the problem is given as the equation
 * x_1 => .. => x_n => X (DB [k_0]) ... (DB [k_n]) =~ x_1 => .. => x_n => [te]
 *  and where [k_lst] = [k_0::k_1::..::k_m].
 *)

let solve (n:int) (k_lst:int LList.t) (te:term) : term =
  let rec add_lam te = function
    | [] -> te
    | _::lst -> add_lam (mk_Lam dloc qmark None te) lst
  in
  add_lam (update_dbs n k_lst te) (LList.lst k_lst)
