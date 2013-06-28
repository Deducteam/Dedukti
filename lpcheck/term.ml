
open Types
open Hashcons

(* Hashcons *)

module T = struct
  type t = term_node
  let equal x y = match x,y with
    | Type , Type               -> true
    | GVar (m,v) , GVar (m',v') -> m=m' && v=v'
    | DB i1 , DB i2             -> i1 = i2
    | App (f1,u1), App (f2,u2)  -> f1.tag = f2.tag && u1.tag = u2.tag
    | Lam (a1,f1) , Lam (a2,f2) -> a1.tag = a2.tag && f1.tag = f2.tag
    | Pi (a1,b1) , Pi (a2,b2)   -> a1.tag = a2.tag && b1.tag = b2.tag
    | _ ,_                      -> false
  let hash = 
    function
    | Type      -> 1
    | DB i      -> i+2
    | GVar id   -> Hashtbl.hash id + 3
    | App (a,b) -> abs ( 19 * ( 19*a.Hashcons.hkey + b.Hashcons.hkey ) + 5 )
    | Lam (a,b) -> abs ( 19 * ( 19*a.Hashcons.hkey + b.Hashcons.hkey ) + 6 )
    | Pi  (a,b) -> abs ( 19 * ( 19*a.Hashcons.hkey + b.Hashcons.hkey ) + 7 )
end

module H = Hashcons.Make(T)
let ht = H.create 3571

let htype       = H.hashcons ht Type
let hgvar id    = H.hashcons ht (GVar id)
let hvar i      = H.hashcons ht (DB i)
let happ (f,u)  = H.hashcons ht (App (f,u))
let hlam (a,t)  = H.hashcons ht (Lam (a,t))  
let hpi (a,b)   = H.hashcons ht (Pi (a,b))

let rec full_hcons (te:term) : term = 
  match te.node with  
    | Type        -> htype
    | GVar id     -> hgvar id
    | DB i        -> hvar i
    | App (a,b)   -> happ ( full_hcons a , full_hcons b )
    | Lam (a,b)   -> hlam ( full_hcons a , full_hcons b )
    | Pi (a,b)    -> hpi  ( full_hcons a , full_hcons b )
 
(* Memoization *)

module M1 = Hashtbl.Make(struct type t = int let equal a b = a = b let hash = Hashtbl.hash end)
module M2 = Hashtbl.Make(struct type t = int*int let equal (a,b) (x,y) = a = x && b = y let hash = Hashtbl.hash end)
module M3 = Hashtbl.Make(struct type t = int*int*int let equal (a,b,c) (x,y,z) = a = x && b = y && c = z let hash = Hashtbl.hash end)

let memo_subst0  : term M3.t = M3.create 3571
let memo_shift  : term M2.t = M2.create 3571
let memo_is_conv : bool M2.t = M2.create 3571
let memo_wnf : term M1.t = M1.create 3571

(* ... *)

let rec of_pterm0 (ctx:(string*int) list) (n:int) = function 
    | PType                     -> htype
    | PApp (f,u)                -> happ ( of_pterm0 ctx n f , of_pterm0 ctx n u )
    | PLam (v,None,t)           -> failwith "not implemented" 
    | PLam ((_,v),Some a,t)     -> hlam ( of_pterm0 ctx n a , of_pterm0 ((v,n)::ctx) (n+1) t )
    | PPi (None,a,b)            -> hpi  ( of_pterm0 ctx n a , of_pterm0 ctx (n+1) b )
    | PPi (Some (l,v),a,b)      -> hpi  ( of_pterm0 ctx n a , of_pterm0 ((v,n)::ctx) (n+1) b )
    | PId (_,m,v)               -> 
        if m = !Global.name then
          ( try hvar (n-(List.assoc v ctx)-1)
            with Not_found -> hgvar (m,v) )
        else
          hgvar (m,v)

let of_pterm te = of_pterm0 [] 0 te

(* Substitution *)

let rec shift (k:int) (t:term) : term = 
  try M2.find memo_shift (k,t.tag) 
  with
      Not_found ->
        let res =
          match t.node with
            | Type              -> failwith "shift (Type)"
            | GVar id           -> hgvar id
            | DB n              -> if n<k then hvar n else hvar (n+1)
            | App (f,a)         -> happ (shift k f,shift k a)
            | Lam (a,f)         -> hlam (shift k a,shift (k+1) f)
            | Pi  (a,b)         -> hpi  (shift k a,shift (k+1) b)
        in
          M2.add memo_shift (k,t.tag) res ; res

let rec shift_lst = function
  | []          -> []
  | a::lst      -> (shift 0 a)::(shift_lst lst)

let rec subst0 (k:int) (u:term) (t:term) : term = 
  try M3.find memo_subst0 (k,t.tag,u.tag)
  with
      Not_found -> 
        let res =
          match t.node with
            | Type              -> htype
            | GVar id           -> hgvar id
            | DB n when k=n     -> u
            | DB n              -> if n>k then hvar (n-1) else hvar n
            | App (f,a)         -> happ (subst0 k u f,subst0 k u a)
            | Lam (a,f)         -> hlam (subst0 k u a,subst0 (k+1) (shift 0 u) f)
            | Pi (a,b)          -> hpi  (subst0 k u a,subst0 (k+1) (shift 0 u) b)
        in
          M3.add memo_subst0 (k,t.tag,u.tag) res ; res

let subst (t:term) (u:term) : term = 
  subst0 0 u t 

let subst_lst (u,lst) =
  let rec aux = function
    | []        -> []
    | a::l      -> (subst a u)::(aux l)
  in
    aux lst

let rec wnf (t:term) : term = 
  try M1.find memo_wnf t.tag
  with Not_found ->
    let res =
      match t.node with
        | App (f,u) -> 
            ( match (wnf f).node with
                | Lam (_,b) -> wnf (subst b u)
                | _         -> t )
        | GVar id   -> 
            ( match Env.get_def id with
                | Env.NoNe    -> t
                | Env.SoMe d  -> wnf d
                | Env.Ext d   -> wnf (full_hcons d)
            )
        | _         -> t
    in 
      M1.add memo_wnf t.tag res ; res

let rec is_conv t1 t2 = 
  if t1.tag = t2.tag then true
  else
    begin
      try M2.find memo_is_conv (t1.tag,t2.tag)
      with Not_found ->
        let res =
          let t1' = wnf t1 in
          let t2' = wnf t2 in
            if t1'.tag = t2'.tag then true
            else
              begin
                match t1'.node,t2'.node with
                  | ( App (f,a) , App (f',a') )      -> is_conv a a' && is_conv f f'
                  | ( Lam (a,f) , Lam (a',f') )      -> is_conv a a' && is_conv f f'
                  | ( Pi  (a,b) , Pi  (a',b') )      -> is_conv a a' && is_conv b b'
                  | ( a , b )                        -> false 
              end
        in
          M2.add memo_is_conv (t1.tag,t2.tag) res ; res
    end
 




