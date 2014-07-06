open Types

(* *** Substitution *** *)

let rec shift_rec (r:int) (k:int) : term -> term = function
  | DB (_,x,n) as t     -> if n<k then t else mk_DB dloc x (n+r)
  | App (f,a,args)    ->
      mk_App (shift_rec r k f) (shift_rec r k a) (List.map (shift_rec r k) args )
  | Lam (_,x,a,f)       -> mk_Lam dloc x (shift_rec r k a) (shift_rec r (k+1) f)
  | Pi  (_,x,a,b)       -> mk_Pi dloc x (shift_rec r k a) (shift_rec r (k+1) b)
  | t                 -> t

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

let rec psubst (nargs,args:int*term list) (k:int) (t:term) =
  (* assert ( nargs = List.length args ); *)
  match t with
    | Type _ | Kind | Const _ | Meta _  -> t
    | DB (_,x,n) when (n >= (k+nargs))    -> mk_DB dloc x (n-nargs)
    | DB (_,_,n) when (n < k)             -> t
    | DB (_,_,n) (* (k<=n<(k+nargs)) *)   -> shift k ( List.nth args (n-k) )
    | Lam (_,x,a,b)                       ->
        mk_Lam dloc x ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b )
    | Pi  (_,x,a,b)                       ->
        mk_Pi dloc x ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b )
    | App (f,a,lst)                     ->
        mk_App (psubst (nargs,args) k f) (psubst (nargs,args) k a)
          (List.map (psubst (nargs,args) k) lst)

let subst t u = psubst (1,[u]) 0 t

                  (*FIXME*)
let rec subst_q (q,u:int*term) (k:int) = function
  | DB (_,_,n) when (n = q+k)                      -> shift k u
  | Type _ | Kind | Const _ | DB _ | Meta _ as t -> t
  | Lam (_,x,a,b) -> mk_Lam dloc x ( subst_q (q,u) k a ) ( subst_q (q,u) (k+1) b )
  | Pi  (_,x,a,b) -> mk_Pi dloc  x ( subst_q (q,u) k a ) ( subst_q (q,u) (k+1) b )
  | App (f,a,lst) -> mk_App (subst_q (q,u) k f) (subst_q (q,u) k a)
                       ( List.map (subst_q (q,u) k) lst )

(*
let rec subst_meta_rec k s  = function
  | Type | Kind | Const _ | DB _ as t -> t
  | Lam (x,a,b) -> mk_Lam x (subst_meta_rec k s a) (subst_meta_rec (k+1) s b)
  | Pi  (x,a,b) -> mk_Pi  x (subst_meta_rec k s a) (subst_meta_rec (k+1) s b)
  | App lst     -> mk_App ( List.map (subst_meta_rec k s) lst)
  | Meta n as t -> ( try shift k (List.assoc n s) with Not_found -> t )

let subst_meta = subst_meta_rec 0
 *)
