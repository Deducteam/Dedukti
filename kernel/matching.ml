open Basics
open Term

exception NotUnifiable

let permute (dbs:int LList.t) (te:term) : term =
  let size = LList.len dbs in
  let rec find n cpt = function
    | [] -> raise NotUnifiable
    | q::lst -> if q=n then size-1-cpt else find n (cpt+1) lst
  in
  let rec aux k = function
    | Type _ | Kind | Const _ as t -> t
    | DB (_,x,n) as t ->
        if n < k then t
        else
          let n' = find (n-k) 0 (LList.lst dbs) in
            mk_DB dloc x (n'+k)
    | Lam (l,x,a,b) -> mk_Lam dloc x None (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi  dloc x (aux k a) (aux (k+1) b)
    | Let (_,x,a,b) -> mk_Let dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te


(* Find F such that F (DB [k_0]) ... (DB [k_n]) =~ [te]
 * when the k_i are distinct *)
let resolve (k_lst:int LList.t) (te:term) : term =
  let rec add_lam te = function
    | [] -> te
    | _::lst -> add_lam (mk_Lam dloc qmark None te) lst
  in
    add_lam (permute k_lst te) (LList.lst k_lst)
