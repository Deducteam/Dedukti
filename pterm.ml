(* TODO delete
open Types

let pos x ctx = 
  let rec aux n = function
    | []                -> None
    | (_,y,_)::l        -> if ident_eq x y then Some n else aux (n+1) l
  in
    aux 0 ctx

let rec of_pterm ctx = function 
  | PType l                     -> mk_type l
  | PId (l,v)                   -> 
      begin
        match pos v ctx with
          | None        -> mk_gvar l !Global.name v
          | Some n      -> mk_db l v n 
      end
  | PQid (l,m,v)                -> mk_gvar l m v
  | PApp args                   -> mk_uapp (List.map (of_pterm ctx) args) 
  | PLam (v,None,t)             -> failwith "Not implement (untyped lambda)." 
  | PPi (None,a,b)              -> 
      let aa = of_pterm ctx a in mk_pi (get_loc aa) empty aa ( of_pterm ((dloc,empty,mk_kind)::ctx) b )
  | PPi (Some (l,v),a,b)        -> mk_pi  l v ( of_pterm ctx a ) ( of_pterm ((dloc,v,mk_kind)::ctx) b )
  | PLam ((l,v),Some a,t)       -> mk_lam l v ( of_pterm ctx a ) ( of_pterm ((dloc,v,mk_kind)::ctx) t )

let of_pcontext (env:pcontext) : context=
  List.fold_left (fun ctx (l,x,ty) -> (l,x,of_pterm ctx ty)::ctx) [] env

let of_ppat ctx p = 
  let cpt = ref (-1) in
  let rec aux = function
    | PDash l           -> ( incr cpt ; Dash (l,!cpt) )
    | PPat ((l,m,v),ps) ->
        begin
          if ( Array.length ps = 0 ) then
            if m = !Global.name then
              ( match pos v ctx with
                  | None        -> Pattern ((l,m,v),[||]) 
                  | Some n      -> Var (l,v,n) ) 
            else
              Pattern ((l,m,v),[||])
          else 
            Pattern ((l,m,v),Array.map aux ps)
        end
  in aux p

let of_ptop ctx ((l,cst),args)  = 
  match of_ppat ctx (PPat ((l,!Global.name,cst),args)) with
    | Pattern (_,pats) -> ((l,cst),pats)
    | _                 -> raise ( PatternError ( l , "The left-hand side of a rule cannot be a variable." ) )
 *)

