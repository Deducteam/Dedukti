open Basics
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

(* let pp_klist out lst = *)
(*   List.iter (fun i -> Printf.fprintf out "?[%i]" i) (LList.lst lst) *)

(* Find F such that F (DB [k_0]) ... (DB [k_n]) =~ [te]
 * F can contain free variable but these should not in [DB 0..DB depth-1]
 * *)
let solve (depth:int) (k_lst:int LList.t) (te:term) : term =
  let rec add_lam te = function
    | [] -> te
    | _::lst -> add_lam (mk_Lam dloc qmark None te) lst
  in
  add_lam (update_dbs depth k_lst te) (LList.lst k_lst)
