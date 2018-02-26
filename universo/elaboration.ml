let rec elaboration sg term =
  let open Term in
  let open Constraints.ReverseCiC in
  if is_prop term then
    term
  else if  is_type term then
    Constraints.UVar.fresh_uvar sg
  else
    match term with
    | App(f, a, al) ->
      let f' = elaboration sg f in
      let a' = elaboration sg a in
      let al' = List.map (elaboration sg) al in
      mk_App f' a' al'
    | Lam(loc, id, t_opt, t) ->
      let t' = elaboration sg t in
      begin
        match t_opt with
        | None -> mk_Lam loc id t_opt t'
        | Some x -> let x' = elaboration sg x in
          mk_Lam loc id (Some x') t'
      end
    | Pi(loc, id, ta, tb) ->
      let ta' = elaboration sg ta in
      let tb' = elaboration sg tb in
      mk_Pi loc id ta' tb'
    | _ ->     term

let elaboration sg e =
  let open Rule in
  let open Parser in
  match e with
  | Decl(l,id,st,t) -> Decl(l,id,st, elaboration sg t)
  | Def(l,id,op,pty,te) -> Def(l,id,op, Basic.map_opt (elaboration sg) pty, elaboration sg te)
  | Rules(rs) ->
    let rs' = List.map (fun (r: untyped_rule) -> {r  with rhs = elaboration sg r.rhs}) rs in
    Rules(rs')
  | Name (l,id) -> Name(l,id)
  | _ -> failwith "unsupported"
