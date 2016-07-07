open Basics
open Term

type ho_env = (int*term) LList.t

exception NotUnifiable

let permute (p:int) (dbs:int LList.t) (te:term) : term =
  let size = LList.len dbs in
  let rec find n cpt = function
    | [] -> ( raise NotUnifiable )
    | q::lst -> if q=n then size-1-cpt else find n (cpt+1) lst
  in
  let rec aux k = function
    | Type _ | Kind | Const _ as t -> t
    | DB (_,x,n) as t ->
        if (n < k || n>=k+p) then t
        else
          let n' = find (n-k) 0 (LList.lst dbs) in
            mk_DB dloc x (n'+k)
    | Lam (l,x,a,b) -> mk_Lam dloc x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi  dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te

(* let pp_klist out lst = *)
(*   List.iter (fun i -> Printf.fprintf out "?[%i]" i) (LList.lst lst) *)

(* Find F such that F (DB [k_0]) ... (DB [k_n]) =~ [te]
 * when the k_i are distinct *)
let resolve (depth:int) (k_lst:int LList.t) (te:term) : term =
  let rec add_lam te = function
    | [] -> te
    | _::lst -> add_lam (mk_Lam dloc qmark None te) lst
  in
  let res = add_lam (permute depth k_lst te) (LList.lst k_lst) in
  res

let rec ho_beta (i:int) (f:term) (args:term list) : term =
  if i <= 0 then
    match args with
    | [] -> f
    | a::lst -> mk_App f a lst
  else
    match f, args with
    | Lam (_,_,_,b), a::lst -> ho_beta (i-1) (Subst.subst b a) lst
    | _, _ -> assert false

let ho_psubst (ctx:ho_env) (t:term) : term =
  let lth = ctx.LList.len in
  let rec aux (k:int) (t:term) : term =
    match t with
    | Type _ | Kind | Const _ -> t
    | DB (_,x,n) when (n >= (k+lth)) -> mk_DB dloc x (n-lth)
    | DB (_,_,n) when (n < k) -> t
    | DB (_,_,n) (* (k<=n<(k+nargs)) *) ->
      let (i,u) = LList.nth ctx (n-k) in
      ( assert (i == 0); Subst.shift k u )
    | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi dloc x (aux k a) (aux (k+1) b)
    | App (DB(_,_,n),a,lst) when ( (k<=n) && (n<(k+lth)) ) ->
      let (i,u) = LList.nth ctx (n-k) in
      let f2 = Subst.shift k u in
      let a2 = aux k a in
      let lst2 = List.map (aux k) lst in
(*       debug "### i=%i - f2=%a - a2=%a - size(lst2)=%i" i pp_term f2 pp_term a2 (List.length lst2); *)
      ho_beta i f2 (a2::lst2)
    | App (f,a,lst)                     ->
      mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in
  aux 0 t
