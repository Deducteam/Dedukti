
open Types

(* --- Substitution --- *)

let rec shift (r:int) (k:int) = function
  | DB n        -> if n<k then DB n else DB (n+r)
  | App args    -> App ( List.map (shift r k) args )
  | Lam (a,f)   -> Lam (shift r k a,shift r (k+1) f)
  | Pi  (a,b)   -> Pi  (shift r k a,shift r (k+1) b)
  | t           -> t 

let rec pop n = function (* n < size of the list *)
  | []                  -> assert false
  | a::_ when n=0       -> a
  | _::lst              -> pop (n-1) lst

(* nargs == List.length args *)
let rec psubst_l (nargs,args) k t =  
  match t with
    | Type | Kind | GVar _ | LVar _     -> t
    | DB n when (n >= (k+nargs))        -> DB (n-nargs)
    | DB n when (n < k)                 -> DB n
    | DB n (* when (k<=n<(k+nargs)) *)  -> shift k 0 (Lazy.force (pop (n-k) args))
    | Lam (a,b)                         -> Lam ( psubst_l (nargs,args) k a , psubst_l (nargs,args) (k+1) b )
    | Pi  (a,b)                         -> Pi  ( psubst_l (nargs,args) k a , psubst_l (nargs,args) (k+1) b ) 
    | App []                            -> assert false
    | App (he::tl)                      ->
        let tl' = List.map (psubst_l (nargs,args) k) tl in
          ( match psubst_l (nargs,args) k he with
              | App tl0 -> App (tl0@tl')
              | he'     -> App (he'::tl') )

(* nargs == List.length args *)
let rec psubst (nargs,args) k t =  
  match t with
    | Type | Kind | GVar _ | LVar _     -> t
    | DB n when (n >= (k+nargs))        -> DB (n-nargs)
    | DB n when (n < k)                 -> DB n
    | DB n (* when (k<=n<(k+nargs)) *)  -> shift k 0 (pop (n-k) args)
    | Lam (a,b)                         -> Lam ( psubst (nargs,args) k a , psubst (nargs,args) (k+1) b )
    | Pi  (a,b)                         -> Pi  ( psubst (nargs,args) k a , psubst (nargs,args) (k+1) b ) 
    | App []                            -> assert false
    | App (he::tl)                      ->
        let tl' = List.map (psubst (nargs,args) k) tl in
          ( match psubst (nargs,args) k he with
              | App tl0 -> App (tl0@tl')
              | he'     -> App (he'::tl') )

let subst t u = psubst (1,[u]) 0 t


