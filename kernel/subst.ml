open Basic
open Format
open Term

exception UnshiftExn

type 'a substitution = 'a -> loc -> ident -> int -> int -> term

let apply_subst (subst:'a substitution) : 'a -> int -> term -> term =
  let ct = ref 0 in
  let rec aux arg k t = match t with  (* k counts the number of local lambda abstractions *)
    | DB (l,x,n) when n >= k ->  (* a free variable *)
       ( try let res = subst arg l x n k in incr ct; res with Not_found -> t)
    | App (f,a,args) ->
      let ct' = !ct in
      let f',a',args' = aux arg k f, aux arg k a, List.map (aux arg k) args in
      if !ct = ct' then t else mk_App f' a' args'
    | Lam (l,x,a,f)  ->
      let ct' = !ct in
      let a',f' = map_opt (aux arg k) a, aux arg (k+1) f in
      if !ct = ct' then t else mk_Lam l x a' f'
    | Pi  (l,x,a,b)  ->
      let ct' = !ct in
      let a',b' = aux arg k a, aux arg (k+1) b in
      if !ct = ct' then t else mk_Pi l x a' b'
    | _ -> t
  in aux
[@@inline]

let shift_rec : int -> int -> term -> term =
  let subst r l x n _ = mk_DB l x (n+r) in
  apply_subst subst

let shift r t = if r = 0 then t else shift_rec r 0 t

(* All free variables are shifted down by [q].
   If their index is less than [q], raises UnshiftExn. *)
let unshift : int -> term -> term =
  let subst q l x n k = if (n-k) < q then raise UnshiftExn else mk_DB l x (n-q) in
  fun k -> apply_subst subst k 0

let psubst_l_ (nargs,get) l x n k =
  if n >= k + nargs then mk_DB l x (n-nargs)
  else get (n-k) k
let psubst_l__ = apply_subst psubst_l_
let psubst_l (args:(term Lazy.t) LList.t) : term -> term =
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
  in psubst_l__ (nargs,get) 0

let subst : term -> term -> term =
  let subst u l x n k = if n = k then shift k u else mk_DB l x (n-1) in
  let aux = apply_subst subst in
  fun te u -> aux u 0 te

let subst_n : int -> ident -> term -> term =
  let subst (m,y) l x n k = if n-k = m then mk_DB l y k else mk_DB l x (n+1) in
  let aux = apply_subst subst in
  fun m y -> aux (m,y) 0


let occurs (n:int) (te:term) : bool =
  let rec aux depth = function
    | Kind | Type _ | Const _ -> false
    | DB (_,_,k) -> k = n + depth
    | App (f,a,args) -> List.exists (aux depth) (f::a::args)
    | Lam (_,_,None,te) -> aux (depth+1) te
    | Lam (_,_,Some ty,te) -> aux depth ty || aux (depth+1) te
    | Pi (_,_,a,b) -> aux depth a || aux (depth+1) b
  in aux 0 te

module IntMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end)

module Subst =
struct
  type t = term IntMap.t
  let identity = IntMap.empty

  let is_identity = IntMap.is_empty

  let subst  sigma     _ _ n k = shift k (IntMap.find (n-k) sigma)
  let subst2 (sigma,i) _ _ n k = shift k (unshift (i+1) (IntMap.find (n+i+1-k) sigma))

  let apply_ = apply_subst subst
  let apply : t -> int -> term -> term =
    fun sigma ->  if is_identity sigma then (fun _ t -> t) else apply_ sigma

  let add (sigma:t) (n:int) (t:term) : t =
    assert ( not (IntMap.mem n sigma) );
    IntMap.add n t sigma

  let rec mk_idempotent (sigma:t) : t =
    let sigma2:t = IntMap.map (apply sigma 0) sigma in
    if IntMap.equal term_eq sigma sigma2 then sigma
    else mk_idempotent sigma2

  let pp (name:(int->ident)) (fmt:formatter) (sigma:t) : unit =
    let pp_aux i t = fprintf fmt "  %a[%i] -> %a\n" pp_ident (name i) i pp_term t in
    IntMap.iter pp_aux sigma

end
