open Basic
open Format
open Term

type ex_substitution = loc -> ident -> int -> int -> int -> term

let apply_exsubst (subst:ex_substitution) (n:int) (te:term) : term*bool =
  let ct = ref 0 in
  (* aux increments this counter every time a substitution occurs.
   * Terms are reused when no substitution occurs in recursive calls. *)
  let rec aux k t = match t with  (* k counts the number of local lambda abstractions *)
    | DB (l,x,n) when n >= k -> (* a free variable *)
       ( try let res = subst l x n 0 k in incr ct; res with Not_found -> t)
    | App (DB (l,x,n) as db,a,args) when n >= k -> (* an applied free variable *)
      let ct' = !ct in
      let f' =
        try
          let res = subst l x n (1+(List.length args)) k in
          incr ct;
          res
        with Not_found -> db in
      let a', args' = aux k a, List.map (aux k) args in
      if !ct = ct' then t else mk_App f' a' args'
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
  in let res = aux n te in (res, !ct > 0)

module IntMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end)

module ExSubst =
struct
  type t = (int*term) IntMap.t
  (* Maps a DB index to an arity and a lambda-lifted substitute *)
  let identity = IntMap.empty
  let is_identity = IntMap.is_empty

  let subst (sigma:t) =
    fun _ _ n nargs k ->
    let (arity,t) = IntMap.find (n-k) sigma in
    if nargs >= arity then Subst.shift k t else raise Not_found

  let subst2 (sigma:t) (i:int) =
    fun _ _ n nargs k ->
    let (argmin,t) = IntMap.find (n+i+1-k) sigma in
    if nargs >= argmin then Subst.shift k (Subst.unshift (i+1) t) else raise Not_found

  let apply (sigma:t) : int -> term -> term*bool =
    if is_identity sigma then (fun _ t -> t,false) else apply_exsubst (subst sigma)

  let add (sigma:t) (n:int) (nargs:int) (t:term) : t =
    assert ( not (IntMap.mem n sigma) || fst (IntMap.find n sigma) > nargs );
    if not (IntMap.mem n sigma)  || fst (IntMap.find n sigma) > nargs
    then IntMap.add n (nargs,t) sigma else sigma

  let applys (sigma:t)  : t -> t*bool =
    if is_identity sigma then (fun t -> t,false)
    else fun sigma' ->
      let modified = ref false in
      let applysigma (n,t) =
        let res,flag = apply sigma 0 t in
        if flag then modified := true;
        (n,res)
      in
      IntMap.map applysigma sigma', !modified

  let rec mk_idempotent (sigma:t) : t =
    let sigma', flag = applys sigma sigma in
    if flag then mk_idempotent sigma' else sigma

  let pp (name:(int->ident)) (fmt:formatter) (sigma:t) : unit =
    let pp_aux i (n,t) =
      fprintf fmt "  %a[%i](%i args) -> %a\n" pp_ident (name i) i n pp_term t in
    IntMap.iter pp_aux sigma

end
