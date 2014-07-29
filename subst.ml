open Types

let rec shift_rec (r:int) (k:int) : term -> term = function
  | DB (_,x,n) as t -> if n<k then t else mk_DB dloc x (n+r)
  | App (f,a,args) ->
      mk_App (shift_rec r k f) (shift_rec r k a) (List.map (shift_rec r k) args )
  | Lam (_,x,a,f) -> mk_Lam dloc x (shift_rec r k a) (shift_rec r (k+1) f)
  | Pi  (_,x,a,b) -> mk_Pi dloc x (shift_rec r k a) (shift_rec r (k+1) b)
  | t -> t

let shift r t = shift_rec r 0 t

let rec psubst_l (nargs,args:int*(term Lazy.t) list) (k:int) (t:term) : term =
  match t with
    | Type _ | Kind | Const _ | Meta _  -> t
    | DB (_,x,n) when (n >= (k+nargs))  -> mk_DB dloc x (n-nargs)
    | DB (_,_,n) when (n < k)           -> t
    | DB (_,_,n) (* (k<=n<(k+nargs)) *) ->
        shift k ( Lazy.force (List.nth args (n-k)) )
    | Lam (_,x,a,b)                     ->
        mk_Lam dloc x (psubst_l (nargs,args) k a) (psubst_l (nargs,args) (k+1) b)
    | Pi  (_,x,a,b)                     ->
        mk_Pi dloc x (psubst_l (nargs,args) k a) (psubst_l (nargs,args) (k+1) b)
    | App (f,a,lst)                     ->
        mk_App (psubst_l (nargs,args) k f) (psubst_l (nargs,args) k a)
          (List.map (psubst_l (nargs,args) k) lst)

let subst (te:term) (u:term) =
  let rec  aux k = function
    | DB (l,x,n) as t ->
        if n = k then shift k u
        else if n>k then mk_DB l x (n-1)
        else (*n<k*) t
    | Type _ | Kind | Const _ | Meta _ as t -> t
    | Lam (_,x,a,b) -> mk_Lam dloc x (aux k a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi dloc  x (aux k a) (aux(k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te
