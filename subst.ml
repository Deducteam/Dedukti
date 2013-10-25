
open Types

(* *** Substitution *** *)

let rec shift (r:int) (k:int) : term -> term = function
  | DB n as t   -> if n<k then t else mk_db (n+r)
  | App (args)  -> mk_uapp (List.map (shift r k) args )
  | Lam (a,f)   -> mk_lam (shift r k a) (shift r (k+1) f)
  | Pi  (a,b)   -> mk_pi  (shift r k a) (shift r (k+1) b)
  | t           -> t 

let rec psubst_l (nargs,args:int*(term Lazy.t) list) (k:int) (t:term) : term =  
  match t with
    | Type | Kind | GVar _ | Meta _     -> t
    | DB n when (n >= (k+nargs))        -> mk_db (n-nargs)
    | DB n when (n < k)                 -> t
    | DB n (* when (k<=n<(k+nargs)) *)  -> shift k 0 ( Lazy.force (List.nth args (n-k)) )
    | Lam (a,b)                         -> mk_lam ( psubst_l (nargs,args) k a ) ( psubst_l (nargs,args) (k+1) b )
    | Pi  (a,b)                         -> mk_pi  ( psubst_l (nargs,args) k a ) ( psubst_l (nargs,args) (k+1) b ) 
    | App lst                           -> mk_app ( List.map (psubst_l (nargs,args) k) lst )

let rec psubst (nargs,args:int*term list) (k:int) (t:term) =  
(* assert ( nargs = List.length args ); *)
  match t with
    | Type | Kind | GVar _ | Meta _     -> t
    | DB n when (n >= (k+nargs))        -> mk_db (n-nargs)
    | DB n when (n < k)                 -> t
    | DB n (* when (k<=n<(k+nargs)) *)  -> shift k 0 ( List.nth args (n-k) )
    | Lam (a,b)                         -> mk_lam ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b )
    | Pi  (a,b)                         -> mk_pi  ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b ) 
    | App lst                           -> mk_app ( List.map (psubst (nargs,args) k) lst )

let subst (t:term) (u:term) : term = psubst (1,[u]) 0 t
