open Basic
open Format
open Term

exception UnshiftExn

let rec shift_rec (r:int) (k:int) : term -> term = function
  | DB (_,x,n) as t -> if n<k then t else mk_DB dloc x (n+r)
  | App (f,a,args) ->
      mk_App (shift_rec r k f) (shift_rec r k a) (List.map (shift_rec r k) args )
  | Lam (_,x,a,f) -> mk_Lam dloc x (map_opt (shift_rec r k) a) (shift_rec r (k+1) f)
  | Pi  (_,x,a,b) -> mk_Pi dloc x (shift_rec r k a) (shift_rec r (k+1) b)
  | t -> t

let shift r t = if r = 0 then t else shift_rec r 0 t

let unshift q te =
  let rec aux k = function
  | DB (_,_,n) as t when n<k -> t
  | DB (l,x,n) ->
    if (n-k) < q then raise UnshiftExn
    else mk_DB l x (n-q)
  | App (f,a,args) -> mk_App (aux k f) (aux k a) (List.map (aux k) args)
  | Lam (l,x,None,f) -> mk_Lam l x None (aux (k+1) f)
  | Lam (l,x,Some a,f) -> mk_Lam l x (Some (aux k a)) (aux (k+1) f)
  | Pi  (l,x,a,b) -> mk_Pi l x (aux k a) (aux (k+1) b)
  | Type _ | Kind | Const _ as t -> t
  in
  aux 0 te

let psubst_l (args:(term Lazy.t) LList.t) (te:term) : term =
  let nargs = args.LList.len in
  let tab = Array.make nargs [] in
  let rec get i k =
    let l = tab.(i) in
    try List.assoc k l
    with Not_found ->
      if k == 0 then Lazy.force (LList.nth args i)
      else
        let res = shift k (get i 0) in
        tab.(i) <- (k,res) :: l;
        res
  in
  let rec aux k t = match t with
    | Type _ | Kind | Const _ -> t
    | DB (_,x,n) when n >= (k+nargs) -> mk_DB dloc x (n-nargs)
    | DB (_,_,n) when n < k          -> t
    | DB (_,_,n) (* k<=n<k+nargs *)  -> get (n-k) k
    | Lam (_,x,a,b)                  ->
        mk_Lam dloc x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (_,x,a,b)                  ->
        mk_Pi dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst)                  ->
        mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te

let subst (te:term) (u:term) =
  let rec  aux k = function
    | DB (l,x,n) as t ->
        if n = k then shift k u
        else if n>k then mk_DB l x (n-1)
        else (*n<k*) t
    | Type _ | Kind | Const _ as t -> t
    | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi dloc  x (aux k a) (aux(k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te

let subst_n n y t =
  let rec aux k t =  match t with
    | Type _ | Kind | Const _ -> t
    | DB (_,_,m) when (m < k) -> t
    | DB (l,x,m) when (m == (n+k)) -> mk_DB l y k
    | DB (l,x,m) (* ( k <= m ) && m != n+k) *) -> mk_DB l x (m+1)
    | Lam (_,x,a,b) -> mk_Lam dloc x (map_opt (aux k) a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 t

module IntMap = Map.Make(
struct
  type t = int
  let compare = compare
end)


(* Subst is only used inside the typing modules. I think it is only used to apply
 * substituion on Miller patterns *)
module Subst =
struct
  (* FIXME: why do we need a (ident * term) IntMap.t and not just a term IntMap.t, for debug? *)
  type t = (ident*term) IntMap.t
  let identity = IntMap.empty

  (* q is a free index that corresponds to the order of the free variable inside the context of a rule *)
  let apply (sigma:t) (te:term) (q:int) : term =
    let rec aux q = function
      | Kind | Type _ | Const _ as t -> t
      | DB (_,_,k) as t when k<q -> t
      | DB (_,_,k) as t (*when k>=q*) ->
          begin
            try shift q (snd (IntMap.find (k-q) sigma))
            with Not_found -> t
          end
      | App (f,a,args) -> mk_App (aux q f) (aux q a) (List.map (aux q) args)
      | Lam (l,x,Some ty,te) -> mk_Lam l x (Some (aux q ty)) (aux (q+1) te)
      | Lam (l,x,None,te) -> mk_Lam l x None (aux (q+1) te)
      | Pi (l,x,a,b) -> mk_Pi l x (aux q a) (aux (q+1) b)
    in
      aux q te

  let occurs (n:int) (te:term) : bool =
    let rec aux q = function
      | Kind | Type _ | Const _ -> false
      | DB (_,_,k) when k<q -> false
      | DB (_,_,k) (*when k>=q*) -> ( k-q == n )
      | App (f,a,args) -> List.exists (aux q) (f::a::args)
      | Lam (_,_,None,te) -> aux (q+1) te
      | Lam (_,_,Some ty,te) -> aux q ty || aux (q+1) te
      | Pi (_,_,a,b) -> aux q a || aux (q+1) b
    in aux 0 te

  let is_identity = IntMap.is_empty
  (* TODO: put this inside pp *)
  let pp (fmt:formatter) (sigma:t) : unit =
    IntMap.iter (fun i (x,t) ->
        fprintf fmt "( %a[%i] = %a )" pp_ident x i pp_term t
      ) sigma

  let add (sigma:t) (x:ident) (n:int) (t:term) : t option =
    assert ( not ( IntMap.mem n sigma ) );
    if occurs n t then None
    else Some ( IntMap.add n (x,t) sigma )

  let rec mk_idempotent (sigma:t) : t =
    let sigma2:t = IntMap.map (fun (x,te) -> (x,apply sigma te 0)) sigma in
    if IntMap.equal (fun a b -> term_eq (snd a) (snd b)) sigma sigma2 then sigma
    else mk_idempotent sigma2

  let fold = IntMap.fold
  let iter = IntMap.iter

end
