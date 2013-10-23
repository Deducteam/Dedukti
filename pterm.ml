
open Types

let pos (x:string) (lst:string list) : int option = 
  let rec aux n = function
    | []                  -> None
    | y::lst when x=y     -> Some n
    | y::lst              -> aux (n+1) lst
  in
    aux 0 lst

let rec of_pterm (ctx:string list) : pterm -> term = function 
  | PType _                     -> Type
  | PId (_,v)                   -> 
      ( match pos v ctx with
          | None        -> GVar (!Global.name,v)
          | Some n      -> DB n )
  | PQid (_,m,v)                -> GVar (m,v)
  | PApp (f,u) as t             -> App ( get_app_lst ctx t [] )
  | PLam (v,None,t)             -> failwith "Not implement (untyped lambda)." 
  | PPi (None,a,b)              -> Pi  ( of_pterm ctx a , of_pterm (""::ctx) b )
  | PPi (Some (l,v),a,b)        -> Pi  ( of_pterm ctx a , of_pterm (v::ctx) b )
  | PLam ((_,v),Some a,t)       -> Lam ( of_pterm ctx a , of_pterm (v::ctx) t )
and get_app_lst ctx t args =
  match t with
    | PApp (f,u)        -> get_app_lst ctx f ((of_pterm ctx u)::args)
    | _                 -> (of_pterm ctx t)::args

let pat_of_ppat (names:string list) (p:ppattern) : pattern = 
  let cpt = ref (-1) in
  let rec aux = function
    | PDash                -> ( incr cpt ; Dash (!cpt) )
    | PPat ((_,m,v),ps) ->
        begin
          if ( Array.length ps = 0 ) then
            if m = !Global.name then
              ( match pos v  names with
                  | None        -> Pattern ((m,v),[||]) 
                  | Some n      -> Var n ) 
            else
              Pattern ((m,v),[||])
          else 
            Pattern ((m,v),Array.map aux ps)
        end
  in aux p

let top_of_ptop (names:string list) ((l,cst),args:ptop) : top = 
  match pat_of_ppat names (PPat ((l,!Global.name,cst),args)) with
    | Pattern (id,pats) -> (id,pats)
    | _                 -> assert false
