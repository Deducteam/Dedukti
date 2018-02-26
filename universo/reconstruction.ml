type model = Basic.ident -> Term.term

let rec reconstruction model term =
  let open Term in
  if Constraints.UVar.is_uvar term then
    let var = Constraints.UVar.ident_of_uvar term in
    model var
  else
    match term with
    | App(f, a, al) ->
      let f' = reconstruction model f in
      let a' = reconstruction model a in
      let al' = List.map (reconstruction model) al in
      mk_App f' a' al'
    | Lam(loc, id, t_opt, t) ->
      let t' = reconstruction model t in
      begin
        match t_opt with
        | None -> mk_Lam loc id t_opt t'
        | Some x -> let x' = reconstruction model x in
          mk_Lam loc id (Some x') t'
      end
    | Pi(loc, id, ta, tb) ->
      let ta' = reconstruction model ta in
      let tb' = reconstruction model tb in
      mk_Pi loc id ta' tb'
    | _ ->     term

let reconstruction model entry =
  let open Rule in
  let open Entry in
  match entry with
  | Decl(l,id,st,t) -> Decl(l,id,st, reconstruction model t)
  | Def(l,id,op,pty,te) -> Def(l,id,op,
                               Basic.map_opt (reconstruction model) pty, reconstruction model te)
  | Rules(rs) ->
    let rs' = List.map (fun (r: untyped_rule) -> {r  with rhs = reconstruction model r.rhs}) rs in
    Rules(rs')
  | Name (l,id) -> Name(l,id)
  | _ -> failwith "unsupported"
