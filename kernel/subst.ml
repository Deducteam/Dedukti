open Basics
open Term

let rec shift_rec (r:int) (k:int) : term -> term = function
  | DB (_,x,n) as t -> if n<k then t else mk_DB dloc x (n+r)
  | App (f,a,args) ->
      mk_App (shift_rec r k f) (shift_rec r k a) (List.map (shift_rec r k) args )
  | Lam (_,x,_,f) -> mk_Lam dloc x None (shift_rec r (k+1) f)
  | Pi  (_,x,a,b) -> mk_Pi dloc x (shift_rec r k a) (shift_rec r (k+1) b)
  | t -> t

let shift r t = shift_rec r 0 t

exception UnshiftExn
let unshift q te =
  let rec aux k = function
  | DB (_,_,n) as t when n<k -> t
  | DB (l,x,n) ->
    if n < q then raise UnshiftExn
    else mk_DB l x (n-q)
  | App (f,a,args) -> mk_App (aux k f) (aux k a) (List.map (aux k) args)
  | Lam (l,x,None,f) -> mk_Lam l x None (aux (k+1) f)
  | Lam (l,x,Some a,f) -> mk_Lam l x (Some (aux k a)) (aux (k+1) f)
  | Pi  (l,x,a,b) -> mk_Pi l x (aux k a) (aux (k+1) b)
  | Type _ | Kind | Const _ as t -> t
  in
    aux 0 te

let rec psubst_l (args:(term Lazy.t) LList.t) (k:int) (t:term) : term =
  let nargs = args.LList.len in
  match t with
    | Type _ | Kind | Const _ -> t
    | DB (_,x,n) when (n >= (k+nargs))  -> mk_DB dloc x (n-nargs)
    | DB (_,_,n) when (n < k)           -> t
    | DB (_,_,n) (* (k<=n<(k+nargs)) *) ->
        shift k ( Lazy.force (LList.nth args (n-k)) )
    | Lam (_,x,_,b)                     ->
        mk_Lam dloc x None (psubst_l args (k+1) b)
    | Pi  (_,x,a,b)                     ->
        mk_Pi dloc x (psubst_l args k a) (psubst_l args (k+1) b)
    | App (f,a,lst)                     ->
        mk_App (psubst_l args k f) (psubst_l args k a)
          (List.map (psubst_l args k) lst)

let subst (te:term) (u:term) =
  let rec  aux k = function
    | DB (l,x,n) as t ->
        if n = k then shift k u
        else if n>k then mk_DB l x (n-1)
        else (*n<k*) t
    | Type _ | Kind | Const _ as t -> t
    | Lam (_,x,_,b) -> mk_Lam dloc x None (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi dloc  x (aux k a) (aux(k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te

(* replace x[n] by y[0] and shift by one*)
let subst_n n y t =
  let rec aux k t =  match t with
    | Type _ | Kind | Const _ -> t
    | DB (_,_,m) when (m < k) -> t
    | DB (l,x,m) when (m == (n+k)) -> mk_DB l y k
    | DB (l,x,m) (* ( k <= m ) && m != n+k) *) -> mk_DB l x (m+1)
    | Lam (_,x,_,b) -> mk_Lam dloc x None (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 t

module IntMap = Map.Make(
struct
  type t = int
  let compare = compare
end)

module S =
struct
  type t = (ident*term) IntMap.t
  let identity = IntMap.empty

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

  let add (sigma:t) (x:ident) (n:int) (t:term) : t option =
    assert ( not ( IntMap.mem n sigma ) );
    if occurs n t then None
    else Some ( IntMap.add n (x,t) sigma )

  let merge s1 s2 =
    let aux _ b1 b2 = match b1, b2 with
      | None, b | b, None -> b
      | Some b1, Some b2 -> assert false (*FIXME*)
    in
      IntMap.merge aux s1 s2

  let is_identity = IntMap.is_empty

  let pp (out:out_channel) (sigma:t) : unit =
    IntMap.iter (fun i (x,t) ->
        Printf.fprintf out "( %a[%i] = %a )" pp_ident x i pp_term t
      ) sigma
end
