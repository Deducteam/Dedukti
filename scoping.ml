open Types

let underscore = hstring "_"

let rec t_of_pt ctx (pte:preterm) : term =
  match pte with
    | PreType l    -> mk_Type l
    | PreId (l,id) ->
        begin
          try
            let v = IdentMap.find id ctx.ident2var in
            mk_Var l v
          with Not_found ->
            mk_Const l !Global.name id
        end
    | PreQId (l,md,id) -> mk_Const l md id
    | PreApp (f,a,args) ->
        mk_App (t_of_pt ctx f) (t_of_pt ctx a) (List.map (t_of_pt ctx) args)
    | PrePi (l,opt,a,b) ->
        let v_opt, ctx2 = match opt with
          | None -> None, ctx
          | Some id ->
              let v = Var.fresh_of_ident id in
              Some v, ctx_bind_ident ctx id v
        in
        mk_Pi l v_opt (t_of_pt ctx a) (t_of_pt ctx2 b)
    | PreLam  (l,id,a,b) ->
        let v = Var.fresh_of_ident id in
        let ctx2 = ctx_bind_ident ctx id v in
        mk_Lam l v (t_of_pt ctx a) (t_of_pt ctx2 b)
    | PreLet (l,id,a,b) ->
        let v = Var.fresh_of_ident id in
        let ctx2 = ctx_bind_ident ctx id v in
        mk_Let l v (t_of_pt ctx a) (t_of_pt ctx2 b)

let rec p_of_pp ctx (ppat:prepattern) : pattern =
  match ppat with
    | PCondition te -> Brackets (t_of_pt ctx te)
    | PPattern (l,None,id,[]) ->
        begin try
          let v = IdentMap.find id ctx.ident2var in
          Var (l, v)
        with Not_found ->
          Pattern (l,!Global.name,id,[])
        end
    | PPattern (l,None,id,args) ->
        Pattern (l,!Global.name,id,List.map (p_of_pp ctx) args)
    | PPattern (l,Some md,id,args) ->
        Pattern (l,md,id,List.map (p_of_pp ctx) args)
    | PJoker l ->
        let v = Var.fresh_of_ident underscore in
        Joker (l, v)

let empty_ctx = IdentMap.empty

let optimize t =
  let rules = if !Global.cse then [Optim.common_subexpr_elim] else [] in
  Optim.optimize ~rules t

let scope_term ~ctx t =
  let t' = t_of_pt ctx t in
  optimize t'

let scope_pattern ~ctx p = p_of_pp ctx p
