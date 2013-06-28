
open Types

(* ... *)

let rec of_pterm0 (ctx:(string*int) list) (n:int) = function 
    | PType                     -> Type
    | PApp (f,u)                -> App ( of_pterm0 ctx n f , of_pterm0 ctx n u )
    | PLam (v,None,t)           -> failwith "not implemented" 
    | PLam ((_,v),Some a,t)     -> Lam ( of_pterm0 ctx n a , of_pterm0 ((v,n)::ctx) (n+1) t )
    | PPi (None,a,b)            -> Pi  ( of_pterm0 ctx n a , of_pterm0 ctx (n+1) b )
    | PPi (Some (l,v),a,b)      -> Pi  ( of_pterm0 ctx n a , of_pterm0 ((v,n)::ctx) (n+1) b )
    | PId (_,m,v)               -> 
        if m = !Global.name then
          ( try DB (n-(List.assoc v ctx)-1)
            with Not_found -> GVar (m,v) )
        else
          GVar (m,v)

let of_pterm te = of_pterm0 [] 0 te

(* Substitution *)

let rec shift (k:int) = function
    | Type              -> failwith "shift (Type)"
    | GVar (_,_) as t   -> t
    | DB n              -> if n<k then DB n else DB (n+1)
    | App (f,a)         -> App (shift k f,shift k a)
    | Lam (a,f)         -> Lam (shift k a,shift (k+1) f)
    | Pi  (a,b)         -> Pi  (shift k a,shift (k+1) b)

let rec shift_lst = function
  | []          -> []
  | a::lst      -> (shift 0 a)::(shift_lst lst)

let rec subst0 (k:int) (u:term) = function
    | Type              -> Type
    | GVar (_,_) as t   -> t
    | DB n when k=n     -> u
    | DB n              -> if n>k then DB (n-1) else DB n
    | App (f,a)         -> App (subst0 k u f,subst0 k u a)
    | Lam (a,f)         -> Lam (subst0 k u a,subst0 (k+1) (shift 0 u) f)
    | Pi (a,b)          -> Pi  (subst0 k u a,subst0 (k+1) (shift 0 u) b)

let subst t u = subst0 0 u t 

let subst_lst (u,lst) =
  let rec aux = function
    | []        -> []
    | a::l      -> (subst a u)::(aux l)
  in
    aux lst

let rec wnf = function
    | App (f,u) as te   -> 
        ( match wnf f with
            | Lam (_,b) -> wnf (subst b u)
            | _         -> te )
    | GVar (m,v) as te  -> 
        ( match Env.get_def (m,v) with
            | None    -> te
            | Some d  -> wnf d
      )
    | te                   -> te

let rec is_conv t1 t2 = 
  if t1 = t2 then true
  else
    let t1' = wnf t1 in
    let t2' = wnf t2 in
      if t1' = t2' then true
      else
        match t1',t2' with
          | ( App (f,a) , App (f',a') )      -> is_conv a a' && is_conv f f'
          | ( Lam (a,f) , Lam (a',f') )      -> is_conv a a' && is_conv f f'
          | ( Pi  (a,b) , Pi  (a',b') )      -> is_conv a a' && is_conv b b'
          | ( a , b )                        -> false 
 




