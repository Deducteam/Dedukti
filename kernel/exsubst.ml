open Basic
open Format
open Term

type ex_substitution = loc -> ident -> int -> int -> int -> term

let apply_exsubst (subst:ex_substitution) : int -> term -> term =
  let ct = ref 0 in
  (* aux increments this counter every time a substitution occurs.
   * Terms are reused when no substitution occurs in recursive calls. *)
  let rec aux k t = match t with  (* k counts the number of local lambda abstractions *)
    | DB (l,x,n) when n >= k -> (* a free variable *)
       ( try let res = subst l x n 0 k in incr ct; res with Not_found -> t)
    | App (DB (l,x,n),a,args) when n >= k -> (* an applied free variable *)
      ( try
          let res = subst l x n (1+(List.length args)) k in
          incr ct;
          let a', args' = aux k a, List.map (aux k) args in
          mk_App res a' args'
        with Not_found -> t)
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

module IntMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end)

module ExSubst =
struct
  type t = (int*term) IntMap.t
  let identity = IntMap.empty

  let is_identity = IntMap.is_empty

  let subst (sigma:t) =
    fun _ _ n nargs k ->
    let (argmin,t) = IntMap.find (n-k) sigma in
    if nargs >= argmin then Subst.shift k t else raise Not_found
  let subst2 (sigma:t) (i:int) =
    fun _ _ n nargs k ->
    let (argmin,t) = IntMap.find (n+i+1-k) sigma in
    if nargs >= argmin then Subst.shift k (Subst.unshift (i+1) t) else raise Not_found

  let apply (sigma:t) : int -> term -> term =
    if is_identity sigma then (fun _ t -> t) else apply_exsubst (subst sigma)

  let add (sigma:t) (n:int) (nargs:int) (t:term) : t =
(*
    assert ( not (IntMap.mem n sigma) || fst (IntMap.find n sigma) > nargs );
*)
    if not (IntMap.mem n sigma)  || fst (IntMap.find n sigma) > nargs
    then IntMap.add n (nargs,t) sigma else sigma

  let rec mk_idempotent (sigma:t) : t =
    let sigma2:t = IntMap.map (fun (n,t) -> n,apply sigma 0 t) sigma in
    if IntMap.equal (fun (n1,t1) (n2,t2) -> n1 = n2 && term_eq t1 t2) sigma sigma2 then sigma
    else mk_idempotent sigma2

  let pp (name:(int->ident)) (fmt:formatter) (sigma:t) : unit =
    let pp_aux i (n,t) =
      fprintf fmt "  %a[%i](...%i...) -> %a(...)\n" pp_ident (name i) i n pp_term t in
    IntMap.iter pp_aux sigma

end
