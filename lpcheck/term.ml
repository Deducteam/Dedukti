
open Types

(* ... *)


let ls (*Local Scope*) : int SHashtbl.t = SHashtbl.create 47

let rec of_pterm0 (n:int) = function 
  | PType                     -> Type
  | PApp (f,u)                -> App ( of_pterm0 n f , of_pterm0 n u )
  | PLam (v,None,t)           -> failwith "Not implemented (untyped lambda)" 
  | PPi (None,a,b)            -> Pi  ( of_pterm0 n a , of_pterm0 (n+1) b )
  | PPi (Some (l,v),a,b)      -> 
      begin
        SHashtbl.add ls v n ;
        let bb = of_pterm0 (n+1) b in
          SHashtbl.remove ls v ;
          Pi  ( of_pterm0 n a , bb )
      end
  | PLam ((_,v),Some a,t)     -> 
      begin
        SHashtbl.add ls v n ;
        let tt = of_pterm0 (n+1) t in
          SHashtbl.remove ls v ;
          Lam ( of_pterm0 n a , tt )
      end
  | PId (_,m,v)               -> 
      if m = !Global.name then
        ( try DB (n-(SHashtbl.find ls v)-1) 
          with Not_found -> GVar (m,v) )
      else
        GVar (m,v)

let of_pterm te = of_pterm0 0 te

(* Substitution *)

let rec shift (k:int) = function
    | Type              -> Type 
    | GVar (_,_) as t   -> t
    | DB n              -> if n<k then DB n else DB (n+1)
    | App (f,a)         -> App (shift k f,shift k a)
    | Lam (a,f)         -> Lam (shift k a,shift (k+1) f)
    | Pi  (a,b)         -> Pi  (shift k a,shift (k+1) b)

let rec shift2 (r:int) (k:int) = function
    | Type              -> Type 
    | GVar (_,_) as t   -> t
    | DB n              -> if n<k then DB n else DB (n+r)
    | App (f,a)         -> App (shift2 r k f,shift2 r k a)
    | Lam (a,f)         -> Lam (shift2 r k a,shift2 r (k+1) f)
    | Pi  (a,b)         -> Pi  (shift2 r k a,shift2 r (k+1) b)

let rec subst0 (k:int) (u:term) = function
    | Type              -> Type
    | GVar (_,_) as t   -> t
    | DB n when k=n     -> u
    | DB n              -> if n>k then DB (n-1) else DB n
    | App (f,a)         -> App (subst0 k u f,subst0 k u a)
    | Lam (a,f)         -> Lam (subst0 k u a,subst0 (k+1) (shift 0 u) f)
    | Pi (a,b)          -> Pi  (subst0 k u a,subst0 (k+1) (shift 0 u) b)

let subst t u = subst0 0 u t 

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
 




