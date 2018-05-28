let rec reconstruction model term =
  let open Term in
  let open Constraints in
  let open Cic in (* TODO: probably there is going to be a bug that can be fixed here
  if is_cast term then
    let s1, s2, t1,t2, t = extract_cast term in
    let s1' = reconstruction model s1 in
    let s2' = reconstruction model s2 in
    let t1' = reconstruction model t1 in
    let t2' = reconstruction model t2 in
    let t'  = reconstruction model t  in
    mk_cast s1' s2' t1' t2' t'
  else *)
    match term with
    | Const _ when Uvar.is_uvar term ->
        let var = Uvar.name_of_uvar term in
        model var
    | App (f, a, al) ->
        let f' = reconstruction model f in
        let a' = reconstruction model a in
        let al' = List.map (reconstruction model) al in
        mk_App f' a' al'
    | Lam (loc, id, t_opt, t) -> (
        let t' = reconstruction model t in
        match t_opt with
        | None -> mk_Lam loc id t_opt t'
        | Some x ->
            let x' = reconstruction model x in
            mk_Lam loc id (Some x') t' )
    | Pi (loc, id, ta, tb) ->
        let ta' = reconstruction model ta in
        let tb' = reconstruction model tb in
        mk_Pi loc id ta' tb'
    | _ -> term


let reconstruction model entry =
  let open Rule in
  let open Entry in
  match entry with
  | Decl (l, id, st, t) -> Decl (l, id, st, reconstruction model t)
  | Def (l, id, op, pty, te) ->
      Def
        ( l
        , id
        , op
        , Basic.map_opt (reconstruction model) pty
        , reconstruction model te )
  | Rules rs ->
      let rs' =
        List.map
          (fun (r: untyped_rule) -> {r with rhs= reconstruction model r.rhs})
          rs
      in
      Rules rs'
  | Name (l, id) -> Name (l, id)
  | _ -> assert false
