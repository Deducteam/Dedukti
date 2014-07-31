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

let rec p_of_pp (ctx:ident list) (ppat:prepattern) : pattern =
  match ppat with
    | PPattern (l,None,id,args) ->
        begin
          let args2 = List.map (p_of_pp ctx) args in
            match get_db_index ctx id with
              | None -> Pattern (l,!Global.name,id,args2)
              | Some n -> Var (l,id,n,args2)
        end
    | PPattern (l,Some md,id,args) ->
        Pattern (l,md,id,List.map (p_of_pp ctx) args)
    | PLambda (l,x,p) -> Lambda (l,x,p_of_pp (x::ctx) p)
    | PCondition te -> Brackets (t_of_pt ctx te) (*FIXME check for bound var*)
    | PJoker l -> Joker l

let scope_term (ctx:context) (pte:preterm) : term =
  t_of_pt (List.map fst ctx) pte

let scope_pattern (ctx:context) (ppat:prepattern) : pattern =
  p_of_pp (List.map fst ctx) ppat
