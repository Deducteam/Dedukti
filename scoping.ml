open Types

let get_db_index ctx id =
  let rec aux n = function
    | [] -> None
    | x::_ when (ident_eq id x) -> Some n
    | _::lst -> aux (n+1) lst
  in aux 0 ctx

let rec t_of_pt (ctx:ident list) (pte:preterm) : term =
  match pte with
    | PreType l    -> mk_Type l
    | PreId (l,id) ->
        begin
          match get_db_index ctx id with
            | None   -> mk_Const l !Global.name id
            | Some n -> mk_DB l id n
        end
    | PreQId (l,md,id) -> mk_Const l md id
    | PreApp (f,a,args) ->
        mk_App (t_of_pt ctx f) (t_of_pt ctx a) (List.map (t_of_pt ctx) args)
    | PrePi (l,opt,a,b) ->
        let ctx2 = match opt with None -> empty::ctx | Some id -> id::ctx in
          mk_Pi l opt (t_of_pt ctx a) (t_of_pt ctx2 b)
    | PreLam  (l,id,a,b) ->
        mk_Lam l id (t_of_pt ctx a) (t_of_pt (id::ctx) b)
    | PreLet (l,id,a,b) ->
        mk_Let l id (t_of_pt ctx a) (t_of_pt (id::ctx) b)

let rec p_of_pp (ctx:ident list) (ppat:prepattern) : pattern =
  let fresh = ref (List.length ctx) in
  match ppat with
    | PCondition te -> Brackets (t_of_pt ctx te)
    | PPattern (l,None,id,[]) ->
         ( match get_db_index ctx id with
            | None -> Pattern (l,!Global.name,id,[])
            | Some n -> Var (l,id,n) )
    | PPattern (l,None,id,args) ->
        Pattern (l,!Global.name,id,List.map (p_of_pp ctx) args)
    | PPattern (l,Some md,id,args) ->
        Pattern (l,md,id,List.map (p_of_pp ctx) args)
    | PJoker l -> let n = !fresh in ( incr fresh ; Joker (l,n) )

let optimize t =
  let rules = if !Global.cse then [Optim.common_subexpr_elim] else [] in
  Optim.optimize ~rules t

let scope_term (ctx:context) (pte:preterm) : term =
  let t = t_of_pt (List.map fst ctx) pte in
  optimize t

let scope_pattern (ctx:context) (ppat:prepattern) : pattern =
  p_of_pp (List.map fst ctx) ppat
