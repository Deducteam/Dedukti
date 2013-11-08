
open Types

(* *** Substitution *** *)

let rec shift (r:int) (k:int) : term -> term = function
  | DB (l,x,n) as t     -> if n<k then t else mk_db l x (n+r)
  | App args            -> mk_uapp (List.map (shift r k) args )
  | Lam (l,x,a,f)       -> mk_lam l x (shift r k a) (shift r (k+1) f)
  | Pi  (l,x,a,b)       -> mk_pi l x (shift r k a) (shift r (k+1) b)
  | t                   -> t 

let rec psubst_l (nargs,args:int*(term Lazy.t) list) (k:int) (t:term) : term =  
  match t with
    | Type _ | Kind | GVar _ | Meta _   -> t
    | DB (l,x,n) when (n >= (k+nargs))  -> mk_db l x (n-nargs)
    | DB (_,_,n) when (n < k)           -> t
    | DB (_,_,n) (* (k<=n<(k+nargs)) *) -> shift k 0 ( Lazy.force (List.nth args (n-k)) )
    | Lam (l,x,a,b)                     -> mk_lam l x (psubst_l (nargs,args) k a) (psubst_l (nargs,args) (k+1) b)
    | Pi  (l,x,a,b)                     -> mk_pi l x (psubst_l (nargs,args) k a) (psubst_l (nargs,args) (k+1) b) 
    | App lst                           -> mk_app (List.map (psubst_l (nargs,args) k) lst)

let rec psubst (nargs,args:int*term list) (k:int) (t:term) =  
(* assert ( nargs = List.length args ); *)
  match t with
    | Type _ | Kind | GVar _ | Meta _   -> t
    | DB (l,x,n) when (n >= (k+nargs))  -> mk_db l x (n-nargs)
    | DB (_,_,n) when (n < k)           -> t
    | DB (_,_,n) (* (k<=n<(k+nargs)) *) -> shift k 0 ( List.nth args (n-k) )
    | Lam (l,x,a,b)                     -> mk_lam l x ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b )
    | Pi  (l,x,a,b)                     -> mk_pi l x ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b ) 
    | App lst                           -> mk_app ( List.map (psubst (nargs,args) k) lst )

let subst (t:term) (u:term) : term = psubst (1,[u]) 0 t

let rec subst_q (q,u:int*term) (k:int) (t:term) =  
  match t with
    | DB (_,_,n) when (n = q+k) -> shift k 0 u
    | Type _ | Kind | GVar _ 
    | Meta _ | DB _             -> t
    | Lam (l,x,a,b)             -> mk_lam l x ( subst_q (q,u) k a ) ( subst_q (q,u) (k+1) b )
    | Pi  (l,x,a,b)             -> mk_pi  l x ( subst_q (q,u) k a ) ( subst_q (q,u) (k+1) b ) 
    | App lst                   -> mk_app ( List.map (subst_q (q,u) k) lst )


