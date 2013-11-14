
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
    | Type _ | Kind | Const _ | Meta _  -> t
    | DB (x,n) when (n >= (k+nargs))    -> mk_DB x (n-nargs)
    | DB (_,n) when (n < k)             -> t
    | DB (_,n) (* (k<=n<(k+nargs)) *)   -> shift k 0 ( Lazy.force (List.nth args (n-k)) )
    | Lam (x,a,b)                       -> mk_Lam x (psubst_l (nargs,args) k a) (psubst_l (nargs,args) (k+1) b)
    | Pi  (x,a,b)                       -> mk_Pi  x (psubst_l (nargs,args) k a) (psubst_l (nargs,args) (k+1) b)
    | App lst                           -> mk_App (List.map (psubst_l (nargs,args) k) lst)

let rec psubst (nargs,args:int*term list) (k:int) (t:term) =
(* assert ( nargs = List.length args ); *)
  match t with
    | Type _ | Kind | Const _ | Meta _  -> t
    | DB (x,n) when (n >= (k+nargs))    -> mk_DB x (n-nargs)
    | DB (_,n) when (n < k)             -> t
    | DB (_,n) (* (k<=n<(k+nargs)) *)   -> shift k 0 ( List.nth args (n-k) )
    | Lam (x,a,b)                       -> mk_Lam x ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b )
    | Pi  (x,a,b)                       -> mk_Pi  x ( psubst (nargs,args) k a ) ( psubst (nargs,args) (k+1) b )
    | App lst                           -> mk_App ( List.map (psubst (nargs,args) k) lst )

let subst t u = psubst (1,[u]) 0 t

let rec subst_q (q,u:int*term) (k:int) (t:term) =
  match t with
    | DB (_,n) when (n = q+k)           -> shift k 0 u
    | Type _ | Kind | Const _ | DB _ | Meta _ -> t
    | Lam (x,a,b)                       -> mk_Lam x ( subst_q (q,u) k a ) ( subst_q (q,u) (k+1) b )
    | Pi  (x,a,b)                       -> mk_Pi   x ( subst_q (q,u) k a ) ( subst_q (q,u) (k+1) b )
    | App lst                           -> mk_App ( List.map (subst_q (q,u) k) lst )

let rec meta_subst k (lst:(int*term) list) (te:term) : term =
    match te with
      | Kind | Type _ | Const _ | DB _  -> te
      | Meta n                          ->
          ( try shift k 0 (List.assoc n lst)
            with Not_found -> te )
      | App args                        -> mk_App ( List.map (meta_subst k lst) args )
      | Lam (x,a,b)                     -> mk_Lam x ( meta_subst k lst a ) ( meta_subst (k+1) lst b )
      | Pi  (x,a,b)                     -> mk_Pi  x ( meta_subst k lst a ) ( meta_subst (k+1) lst b )
