open Types

let rec shift_rec (r:int) (k:int) : term -> term = function
  | DB (_,x,n) as t -> if n<k then t else mk_DB dloc x (n+r)
  | App (f,a,args) ->
      mk_App (shift_rec r k f) (shift_rec r k a) (List.map (shift_rec r k) args )
  | Lam (_,x,a,f) -> mk_Lam dloc x (shift_rec r k a) (shift_rec r (k+1) f)
  | Pi  (_,x,a,b) -> mk_Pi dloc x (shift_rec r k a) (shift_rec r (k+1) b)
  | Let (_,x,a,b) -> mk_Let dloc x (shift_rec r k a) (shift_rec r (k+1) b)
  | t -> t

let shift r t = shift_rec r 0 t

let rec contains k t = match t with
  | Kind | Type _ | Const _ | Meta _  -> false
  | DB (_, _, n)      -> n=k
  | App (f,a,l)       -> contains k f || contains k a || List.exists (contains k) l
  | Pi (_,_,a,b)
  | Lam (_,_,a,b)
  | Let (_,_,a,b)     -> contains k a || contains (k+1) b

let rec unshift_rec r k t = match t with
  | DB (_,x,n) ->
      if n<k then t
      else if n-k>r then mk_DB dloc x (n-r)
      else failwith "unshift"
  | App (f,a,args) ->
      mk_App (unshift_rec r k f) (unshift_rec r k a) (List.map (unshift_rec r k) args )
  | Lam (_,x,a,f) -> mk_Lam dloc x (unshift_rec r k a) (unshift_rec r (k+1) f)
  | Pi  (_,x,a,b) -> mk_Pi dloc x (unshift_rec r k a) (unshift_rec r (k+1) b)
  | Let (_,x,a,b) -> mk_Let dloc x (unshift_rec r k a) (unshift_rec r (k+1) b)
  | t -> t

let unshift r t = unshift_rec r 0 t

let rec can_unshift_rec r k t = match t with
  | DB (_,x,n) -> n<k || n-k>r
  | App (f,a,args) ->
      can_unshift_rec r k f
      && can_unshift_rec r k a
      && List.for_all (can_unshift_rec r k) args
  | Lam (_,_,a,b)
  | Pi  (_,_,a,b)
  | Let (_,_,a,b) -> can_unshift_rec r k a && can_unshift_rec r (k+1) b
  | t -> true

let can_unshift r t = can_unshift_rec r 0 t

let rec psubst_l (args:(term Lazy.t) LList.t) (k:int) (t:term) : term =
  let nargs = args.LList.len in
  match t with
    | Type _ | Kind | Const _ | Meta _  -> t
    | DB (_,x,n) when (n >= (k+nargs))  -> mk_DB dloc x (n-nargs)
    | DB (_,_,n) when (n < k)           -> t
    | DB (_,_,n) (* (k<=n<(k+nargs)) *) ->
        shift k ( Lazy.force (LList.nth args (n-k)) )
    | Lam (_,x,a,b)                     ->
        mk_Lam dloc x (psubst_l args k a) (psubst_l args (k+1) b)
    | Pi  (_,x,a,b)                     ->
        mk_Pi dloc x (psubst_l args k a) (psubst_l args (k+1) b)
    | App (f,a,lst)                     ->
        mk_App (psubst_l args k f) (psubst_l args k a)
          (List.map (psubst_l args k) lst)
    | Let (_,x,a,b)                     ->
        mk_Let dloc x (psubst_l args k a) (psubst_l args (k+1) b)
        (* psubst_l (LList.cons (Lazy.from_val a) args) (k+1) b *)

let subst (te:term) (u:term) =
  let rec  aux k = function
    | DB (l,x,n) as t ->
        if n = k then shift k u
        else if n>k then mk_DB l x (n-1)
        else (*n<k*) t
    | Type _ | Kind | Const _ | Meta _ as t -> t
    | Lam (_,x,a,b) -> mk_Lam dloc x (aux k a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi dloc  x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
    | Let (_,x,a,b) -> mk_Let dloc x (aux k a) (aux (k+1) b)
  in aux 0 te
