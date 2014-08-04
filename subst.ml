open Types

let rec psubst sigma t =
  match t with
    | Type _ | Kind | Const _ | Meta _  -> t
    | Var (_,v) ->
        begin match subst_find sigma v with
        | None -> t
        | Some t' -> psubst sigma t'
        end
    | Lam (_,v,a,b) ->
        let v' = Var.fresh v in
        let sigma' = subst_bind sigma v (mk_Var dloc v') in
        mk_Lam dloc v' (psubst sigma a) (psubst sigma' b)
    | Pi (_,None,a,b) ->
        mk_Pi dloc None (psubst sigma a) (psubst sigma b)
    | Pi (_,Some v,a,b) ->
        let v' = Var.fresh v in
        let sigma' = subst_bind sigma v (mk_Var dloc v') in
        mk_Pi dloc (Some v') (psubst sigma a) (psubst sigma' b)
    | App (f, a, l) ->
        mk_App (psubst sigma f) (psubst sigma a) (psubst_list sigma l)
and psubst_list sigma l = List.map (psubst sigma) l

let rec psubst_l sigma t =
  match t with
    | Type _ | Kind | Const _ | Meta _  -> t
    | Var (_,v) ->
        begin match subst_find sigma v with
        | None -> t
        | Some (lazy t') -> psubst_l sigma t'
        end
    | Lam (_,v,a,b) ->
        let v' = Var.fresh v in
        let sigma' = subst_bind sigma v (Lazy.from_val (mk_Var dloc v')) in
        mk_Lam dloc v' (psubst_l sigma a) (psubst_l sigma' b)
    | Pi (_,None,a,b) ->
        mk_Pi dloc None (psubst_l sigma a) (psubst_l sigma b)
    | Pi (_,Some v,a,b) ->
        let v' = Var.fresh v in
        let sigma' = subst_bind sigma v (Lazy.from_val (mk_Var dloc v')) in
        mk_Pi dloc (Some v') (psubst_l sigma a) (psubst_l sigma' b)
    | App (f, a, l) ->
        mk_App (psubst_l sigma f) (psubst_l sigma a) (psubst_l_list sigma l)
and psubst_l_list sigma l = List.map (psubst_l sigma) l

let subst t ~var ~by =
  let sigma = subst_bind subst_empty var by in
  psubst sigma t
