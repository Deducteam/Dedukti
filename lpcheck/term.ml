
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

 (*function
    | Type              -> failwith "shift (Type)"
    | GVar (_,_) as t   -> t
    | DB n              -> if n<k then DB n else DB (n+1)
    | App (f,a)         -> App (shift k f,shift k a)
    | Lam (a,f)         -> Lam (shift k a,shift (k+1) f)
    | Pi  (a,b)         -> Pi  (shift k a,shift (k+1) b)
  *)
  (*
  function
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
*)

let mk_shift (t:term) : term = Subst (t,Shift 1)
let mk_shift_lst   lst = List.map (fun a -> mk_shift a) lst
  (*
  let rec aux = function
    | []        -> []
    | a::l      -> (subst a u)::(aux l)
  in
    aux lst
    *)

let rec compose (s:substitution) (t:substitution) : substitution =
  match s, t with
    | s, Shift 0                -> s
    | Dot (e, s), Shift m       -> compose s (Shift (m - 1))
    | Shift m, Shift n          -> Shift (m + n)
    | s, Dot (e, t)             -> Dot (Subst (e,s), compose s t)
    | a , Compose (b,c)         -> compose (Compose (a,b)) c
    | Compose (a,b) ,  c        -> compose a (compose b c) (*two calls ?*)

let rec subst (s:substitution) (e:term) : term =
  match s, e with
    | Shift m, DB k     -> DB (k + m)
    | Dot (e, s), DB 0  -> subst (Shift 0) e
    | Dot (e, s), DB k  -> subst s (DB (k - 1))
    | Compose (s,t) , DB k -> subst (compose s t) e
    | s, Subst (e,t)    -> (*subst s (subst t e)*) subst (Compose (s,t)) e
    | s, Pi (a,b)       -> Pi ( Subst (a,s) , Subst ( b , Dot (DB 0, Compose (Shift 1,s)) ) ) (* subst s a , subst (Dot (DB 0, compose (Shift 1) s)) b  *)
    | s, Lam (a,b)      -> Lam ( Subst (a,s) , Subst ( b , Dot (DB 0, Compose (Shift 1,s)) ) )(* subst s a , subst (Dot (DB 0, compose (Shift 1) s)) b  *)
    | s, App (t,u)      -> App ( Subst (t,s) , Subst(u,s) ) (* subst s t , subst s u*)
    | s, GVar id        -> GVar id
    | s, Type           -> Type

let mk_subst (t:term) (u:term) : term = Subst (t,Dot(u,Shift 0))
let mk_subst_lst u lst = List.map (fun t -> mk_subst t u) lst

let rec wnf (te:term) : term =
  match te with 
    | App (f,u)  -> 
        ( match wnf f with
            | Lam (_,b) -> wnf (mk_subst b u)
            | _         -> te )
    | GVar (m,v)  -> 
        ( match Env.get_def (m,v) with
            | None    -> te
            | Some d  -> wnf d
        )
    | Subst (t,s) -> wnf (subst s t)
    | _           -> te

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
          | ( a , b )                        -> ( Global.msg ("#### "^Debug.string_of_term a^"\n#### "^Debug.string_of_term b^"\n") ; false )
 




