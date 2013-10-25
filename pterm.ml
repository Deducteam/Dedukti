
open Types

let pos (x:ident) (lst:ident list) : int option = 
  let rec aux n = function
    | []        -> None
    | y::l      -> if ident_eq x y then Some n else aux (n+1) l
  in
    aux 0 lst

let rec of_pterm (ctx:ident list) : pterm -> term = function 
  | PType _                     -> mk_type
  | PId (_,v)                   -> 
      ( match pos v ctx with
          | None        -> mk_gvar !Global.name v
          | Some n      -> mk_db n )
  | PQid (_,m,v)                -> mk_gvar m v
  | PApp (f,u)                  -> mk_app [ of_pterm ctx f ; of_pterm ctx u ]
  | PLam (v,None,t)             -> failwith "Not implement (untyped lambda)." 
  | PPi (None,a,b)              -> mk_pi  ( of_pterm ctx a ) ( of_pterm (empty::ctx) b )
  | PPi (Some (l,v),a,b)        -> mk_pi  ( of_pterm ctx a ) ( of_pterm (v::ctx) b )
  | PLam ((_,v),Some a,t)       -> mk_lam ( of_pterm ctx a ) ( of_pterm (v::ctx) t )

let pat_of_ppat (names:ident list) (p:ppattern) : pattern = 
  let cpt = ref (-1) in
  let rec aux = function
    | PDash                -> ( incr cpt ; Dash (!cpt) )
    | PPat ((l,m,v),ps) ->
        begin
          if ( Array.length ps = 0 ) then
            if m = !Global.name then
              ( match pos v  names with
                  | None        -> Pattern ((l,m,v),[||]) 
                  | Some n      -> Var n ) 
            else
              Pattern ((l,m,v),[||])
          else 
            Pattern ((l,m,v),Array.map aux ps)
        end
  in aux p

let top_of_ptop (names:ident list) ((l,cst),args:ptop) : top = 
  match pat_of_ppat names (PPat ((l,!Global.name,cst),args)) with
    | Pattern (id,pats) -> (id,pats)
    | _                 -> raise ( PatternError ( l , "The left-hand side of a rule cannot be a variable." ) )

