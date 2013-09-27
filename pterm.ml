
open Types

let pos x lst = 
  let rec aux n = function
    | []                  -> None
    | y::lst when x=y     -> Some n
    | y::lst              -> aux (n+1) lst
  in
    aux 0 lst

(* Invariant: k == List.length ctx *)      
let rec of_pterm (k:int) (ctx:string list) : pterm -> term = function 
  | PType                       -> Type
  | PId (_,v)                   -> 
      ( match pos v ctx with
          | None        -> GVar (!Global.name,v)
          | Some n      -> DB n )
  | PQid (_,m,v)                -> GVar (m,v)
  | PApp (f,u) as t             -> App ( get_app_lst k ctx t [] )
  | PLam (v,None,t)             -> raise (ParserError "Not implemented (untyped lambda)")
  | PPi (None,a,b)              -> Pi  ( of_pterm k ctx a , of_pterm (k+1) (""::ctx) b )
  | PPi (Some (l,v),a,b)        -> Pi  ( of_pterm k ctx a , of_pterm (k+1) ( v::ctx) b )
  | PLam ((_,v),Some a,t)       -> Lam ( of_pterm k ctx a , of_pterm (k+1) ( v::ctx) t )
and get_app_lst k ctx t args =
  match t with
    | PApp (f,u)        -> get_app_lst k ctx f ((of_pterm k ctx u)::args)
    | _                 -> (of_pterm k ctx t)::args

(* Invariant: k == List.length ctx *)      
let rec term_of_pat (k:int) (ctx:string list) : pattern -> term = function
  | Pat ((_,m,v),ds,ps) ->
      begin
        if Array.length ps=0 && Array.length ds=0 then
          if m = !Global.name then
            ( match pos v ctx with
                | None        -> GVar (!Global.name,v)
                | Some n      -> DB n )
          else
            GVar (m,v)
        else 
          let l1 = Array.fold_right (fun p lst -> (term_of_pat k ctx p)::lst) ps [] in
          let l2 = Array.fold_right (fun t lst -> (of_pterm k ctx t)::lst) ds l1 in
            App ( (GVar (m,v))::l2)
      end

let term_of_tpat k ctx ((l,v),ds,ps:top_pattern) : term = term_of_pat k ctx (Pat ((l,!Global.name,v),ds,ps))


