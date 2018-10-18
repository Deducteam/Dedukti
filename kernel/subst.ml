open Basic
open Format
open Term

exception UnshiftExn

let replace_free replacer =
  let rec aux k = function
    | DB (l,x,n) when n >= k -> replacer l x n k
    | Type _ | Kind | Const _ | DB _ as t -> t
    | App (f,a,args) -> mk_App (aux k f) (aux k a) (List.map (aux k) args)
    | Lam (l,x,a,f)  -> mk_Lam l x (map_opt (aux k) a) (aux (k+1) f)
    | Pi  (l,x,a,b)  -> mk_Pi  l x (aux k a) (aux (k+1) b)
  in aux

let rec shift_rec (r:int) (k:int) : term -> term = replace_free (fun _ x n _ -> mk_DB dloc x (n+r)) 0

let shift r t = if r = 0 then t else shift_rec r 0 t

(* All free variables are shifted down by [q]. If their index is less than [q], raises UnshiftExn. *)
let unshift q =
  replace_free (fun l x n k -> if (n-k) < q then raise UnshiftExn else mk_DB l x (n-q)) 0

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
  in
  let replacer l x n k =
    if n >= k + nargs then mk_DB l x (n-nargs)
    else get (n-k) k
  in
  replace_free replacer 0

let subst (te:term) (u:term) =
  replace_free (fun l x n k -> if n = k then shift k u else mk_DB l x (n-1)) 0 te

let subst_n m y =
  replace_free (fun l x n k -> if n-k = m then mk_DB l y k else mk_DB l x (n+1)) 0

module IntMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end)


let occurs (n:int) (te:term) : bool =
  let rec aux depth = function
    | Kind | Type _ | Const _ -> false
    | DB (_,_,k) -> k = n + depth
    | App (f,a,args) -> List.exists (aux depth) (f::a::args)
    | Lam (_,_,None,te) -> aux (depth+1) te
    | Lam (_,_,Some ty,te) -> aux depth ty || aux (depth+1) te
    | Pi (_,_,a,b) -> aux depth a || aux (depth+1) b
  in aux 0 te

module Subst =
struct
  type t = term IntMap.t
  let identity = IntMap.empty

  let is_identity = IntMap.is_empty

  let apply (sigma:t) : int -> term -> term =
    if is_identity sigma then (fun _ t -> t) else
    replace_free (fun l x n k -> try shift k (IntMap.find (n-k) sigma) with Not_found -> mk_DB l x n)

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
