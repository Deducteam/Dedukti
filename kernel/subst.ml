open Basic
open Format
open Term

exception UnshiftExn

type substitution = loc -> ident -> int -> int -> term

let apply_subst (subst:substitution) : int -> term -> term =
  let ct = ref 0 in
  (* aux increments this counter every time a substitution occurs.
   * Terms are reused when no substitution occurs in recursive calls. *)
  let rec aux k t = match t with
    (* k counts the number of local lambda abstractions *)
    | DB (l,x,n) when n >= k -> (* a free variable *)
       ( try let res = subst l x n k in incr ct; res with Not_found -> t)
    | App (f,a,args) ->
      let ct' = !ct in
      let f', a', args' = aux k f, aux k a, List.map (aux k) args in
      if !ct = ct' then t else mk_App f' a' args'
    | Lam (l,x,a,f) ->
      let ct' = !ct in
      let a', f' = map_opt (aux k) a, aux (k+1) f in
      if !ct = ct' then t else mk_Lam l x a' f'
    | Pi  (l,x,a,b) ->
      let ct' = !ct in
      let a', b' = aux k a, aux (k+1) b in
      if !ct = ct' then t else mk_Pi  l x a' b'
    | _ -> t
  in aux

let shift r t = if r = 0 then t
  else apply_subst (fun l x n _ -> mk_DB l x (n+r)) 0 t

(* All free variables are shifted down by [q]. If their index is less than [q], raises UnshiftExn. *)
let unshift q =
  apply_subst (fun l x n k -> if n < q+k then raise UnshiftExn else mk_DB l x (n-q)) 0

let psubst_l (args:(term Lazy.t) LList.t) : term -> term =
  let nargs = LList.len args in
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
  let subst l x n k =
    if n >= k + nargs then mk_DB l x (n-nargs)
    else get (n-k) k
  in
  apply_subst subst 0

let subst (te:term) (u:term) =
  apply_subst (fun l x n k -> if n = k then shift k u else mk_DB l x (n-1)) 0 te

let subst_n m y =
  apply_subst (fun l x n k -> if n = m+k then mk_DB l y k else mk_DB l x (n+1)) 0

exception Occurs
let occurs (n:int) (te:term) : bool =
  let rec aux depth = function
    | Kind | Type _ | Const _ -> ()
    | DB (_,_,k) -> if k = n + depth then raise Occurs else ()
    | App (f,a,args) -> List.iter (aux depth) (f::a::args)
    | Lam (_,_,None,te) -> aux (depth+1) te
    | Lam (_,_,Some ty,te) -> aux depth ty; aux (depth+1) te
    | Pi (_,_,a,b) -> aux depth a; aux (depth+1) b
  in
  try aux 0 te; false with Occurs -> true

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

  let subst (sigma:t) = fun _ _ n k -> shift k (IntMap.find (n-k) sigma)
  let subst2 (sigma:t) (i:int) = fun _ _ n k -> shift k (unshift (i+1) (IntMap.find (n+i+1-k) sigma))

  let apply (sigma:t) : int -> term -> term =
    if is_identity sigma then (fun _ t -> t) else apply_subst (subst sigma)

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
