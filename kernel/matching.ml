open Basic
open Term

exception NotUnifiable

(* TODO: document this function *)
let update_dbs (depth:int) (dbs:int LList.t) (te:term) : term =
  let size = LList.len dbs in
  let arr = Array.make depth None in
  List.iteri ( fun i n -> arr.(n) <- Some (size-i-1) ) (LList.lst dbs);
  (* TODO: This could be computed once for all at compile time *)
  let rec aux k = function
    | Type _ | Kind | Const _ as t -> t
    | DB (l,x,n) as t ->
        if n < k (* var bound in te *) then t
        else if n >= k+depth (* var free in te *)
        then mk_DB l x (n+size)
        else
          ( match arr.(n-k) with
            | None -> raise NotUnifiable
            | Some n' -> mk_DB dloc x (n'+k) )
    | Lam (l,x,a,b) -> mk_Lam dloc x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi  dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in
  aux 0 te

let rec add_n_lam te n = if n = 0 then te else add_n_lam (mk_Lam dloc dmark None te) (n-1)

let solve (n:int) (k_lst:int LList.t) (te:term) : term =
  Subst.unshift n
    ( if LList.is_empty k_lst then te
      else add_n_lam (update_dbs n k_lst te) (LList.len k_lst) )
(* This could be optimized by doing the unshifting directly in update_dbs.
I suspect the following would work:

-        else if n >= k+depth (* var free in te *)
-        then mk_DB l x (n+size)
+        else if n >= k+depth (* var free in te *)
+        then mk_DB l x (n+size-depth)

 *)
