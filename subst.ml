
open Types

(* *** Substitution *** *)

let rec shift (r:int) (k:int) : term -> term = function
  | DB (x,n) as t     -> if n<k then t else mk_DB x (n+r)
  | App args          -> mk_App (List.map (shift r k) args )
  | Lam (x,a,f)       -> mk_Lam x (shift r k a) (shift r (k+1) f)
  | Pi  (x,a,b)       -> mk_Pi  x (shift r k a) (shift r (k+1) b)
  | t                 -> t

let rec psubst_l (nargs,args:int*(term Lazy.t) list) (k:int) (t:term) : term =
  match t with
    | Type _ | Kind | Const _           -> t
    | DB (x,n) when (n >= (k+nargs))    -> mk_DB x (n-nargs)
    | DB (_,n) when (n < k)             -> t
    | DB (_,n) (* (k<=n<(k+nargs)) *)   -> shift k 0 ( Lazy.force (List.nth args (n-k)) )
    | Lam (x,a,b)                       -> 
        mk_Lam x (psubst_l (nargs,args) k a) (psubst_l (nargs,args) (k+1) b)
    | Pi  (x,a,b)                       -> 
        mk_Pi  x (psubst_l (nargs,args) k a) (psubst_l (nargs,args) (k+1) b)
    | App lst                           -> 
        mk_App (List.map (psubst_l (nargs,args) k) lst)

let rec psubst (nargs,args:int*term list) (k:int) (t:term) =
(* assert ( nargs = List.length args ); *)
  match t with
    | Type _ | Kind | Const _           -> t
    | DB (x,n) when (n >= (k+nargs))    -> mk_DB x (n-nargs)
    | DB (_,n) when (n < k)             -> t
    | DB (_,n) (* (k<=n<(k+nargs)) *)   -> shift k 0 ( List.nth args (n-k) )
    | Lam (x,a,b)                       -> 
        mk_Lam x ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b )
    | Pi  (x,a,b)                       -> 
        mk_Pi  x ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b )
    | App lst                           -> 
        mk_App ( List.map (psubst (nargs,args) k) lst )

let subst t u = psubst (1,[u]) 0 t

let rec subst_q (q,u:int*term) (k:int) (t:term) =
  match t with
    | DB (_,n) when (n = q+k)           -> shift k 0 u
    | Type _ | Kind | Const _ | DB _    -> t
    | Lam (x,a,b)                       -> 
        mk_Lam x ( subst_q (q,u) k a ) ( subst_q (q,u) (k+1) b )
    | Pi  (x,a,b)                       -> 
        mk_Pi   x ( subst_q (q,u) k a ) ( subst_q (q,u) (k+1) b )
    | App lst                           -> 
        mk_App ( List.map (subst_q (q,u) k) lst )

(* Partial Terms *)

let rec shift_pt r k = function
  | Term t              -> mk_partial (shift r k t)
  | Meta _ as t         -> t
  | PartialApp lst      -> mk_partial_app (List.map (shift_pt r k) lst)
  | PartialLam (x,a,b)  -> mk_partial_lam x (shift_pt r k a) (shift_pt r (k+1) b)
  | PartialPi  (x,a,b)  -> mk_partial_pi  x (shift_pt r k a) (shift_pt r (k+1) b)

let rec subst_pt_rec k u = function
  | PartialApp args     -> mk_partial_app (List.map (subst_pt_rec k u) args)
  | PartialLam (x,a,b)  -> mk_partial_lam x (subst_pt_rec k u a) (subst_pt_rec (k+1) u b)
  | PartialPi (x,a,b)   -> mk_partial_pi x (subst_pt_rec k u a) (subst_pt_rec (k+1) u b)
  | Meta n as t         -> t
  | Term (App args)     -> 
      mk_partial_app (List.map (fun a -> subst_pt_rec k u (mk_partial a)) args)
  | Term (Lam (x,a,b))  -> 
      mk_partial_lam x (subst_pt_rec k u (mk_partial a)) (subst_pt_rec (k+1) u (mk_partial b))
  | Term (Pi  (x,a,b))  -> 
      mk_partial_pi  x (subst_pt_rec k u (mk_partial a)) (subst_pt_rec (k+1) u (mk_partial b)) 
  | Term (DB (x,n)) when n>k    -> mk_partial ( mk_DB x (n-1) )
  | Term (DB (x,n)) when n=k    -> shift_pt k 0 u
  | Term (Kind|Type|Const _|DB _) as t -> t 

let subst_pt t u = subst_pt_rec 0 u t 

let rec meta_subst k s = function
  | Term _ as t         -> t
  | Meta n as t         -> 
      ( try shift_pt k 0 (List.assoc n s)
        with Not_found -> t )
  | PartialApp args     -> mk_partial_app (List.map (meta_subst k s) args)
  | PartialPi  (x,a,b)  -> mk_partial_pi  x (meta_subst k s a) (meta_subst (k+1) s b)
  | PartialLam (x,a,b)  -> mk_partial_lam x (meta_subst k s a) (meta_subst (k+1) s b)

let rec meta_subst_pattern s = function
  | Var _ as p                  -> p
  | Dot _ as p                  -> p
  | Joker n as p                -> ( try Dot (List.assoc n s) with Not_found -> p )
  | Pattern (md,id,args)        -> Pattern (md,id,Array.map (meta_subst_pattern s) args)


