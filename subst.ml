
open Types

(* *** Substitution *** *)

let rec shift (r:int) (k:int) : term -> term = function
  | DB n        -> if n<k then DB n else DB (n+r)
  | App args    -> App ( List.map (shift r k) args )
  | Lam (a,f)   -> Lam (shift r k a,shift r (k+1) f)
  | Pi  (a,b)   -> Pi  (shift r k a,shift r (k+1) b)
  | t           -> t 

let rec psubst_l (nargs,args:int*(term Lazy.t) list) (k:int) (t:term) : term =  
  match t with
    | Type | Kind | GVar _              -> t
    | DB n when (n >= (k+nargs))        -> DB (n-nargs)
    | DB n when (n < k)                 -> DB n
    | DB n (* when (k<=n<(k+nargs)) *)  -> shift k 0 (Lazy.force (List.nth args (n-k)))
    | Lam (a,b)                         -> Lam ( psubst_l (nargs,args) k a , psubst_l (nargs,args) (k+1) b )
    | Pi  (a,b)                         -> Pi  ( psubst_l (nargs,args) k a , psubst_l (nargs,args) (k+1) b ) 
    | App []                            -> assert false
    | App (he::tl)                      ->
        let tl' = List.map (psubst_l (nargs,args) k) tl in
          ( match psubst_l (nargs,args) k he with
              | App tl0 -> App (tl0@tl')
              | he'     -> App (he'::tl') )

let rec psubst (nargs,args:int*term list) (k:int) (t:term) =  
(* assert ( nargs = List.length args ); *)
  match t with
    | Type | Kind | GVar _              -> t
    | DB n when (n >= (k+nargs))        -> DB (n-nargs)
    | DB n when (n < k)                 -> DB n
    | DB n (* when (k<=n<(k+nargs)) *)  -> shift k 0 (List.nth args (n-k))
    | Lam (a,b)                         -> Lam ( psubst (nargs,args) k a , psubst (nargs,args) (k+1) b )
    | Pi  (a,b)                         -> Pi  ( psubst (nargs,args) k a , psubst (nargs,args) (k+1) b ) 
    | App []                            -> assert false
    | App (he::tl)                      ->
        let tl' = List.map (psubst (nargs,args) k) tl in
          ( match psubst (nargs,args) k he with
              | App tl0 -> App (tl0@tl')
              | he'     -> App (he'::tl') )

let subst (t:term) (u:term) : term = psubst (1,[u]) 0 t

let subst2 (lst:(int*term) list) (t:term) : term =
  let rec aux (k:int) (te:term) = 
    match te with
      | Kind | Type | GVar _        -> te
      | DB n                        -> 
          ( try List.assoc (n-k) lst
            with Not_found -> DB n )
      | App args                    -> App ( List.map (aux k) args )
      | Lam (a,b)                   -> Lam ( aux k a , aux (k+1) b )
      | Pi  (a,b)                   -> Pi  ( aux k a , aux (k+1) b )
  in
    aux 0 t


