open Basic
open Term
open Cic
open Uvar

let mk_type0 = mk_type mk_z

let extract_prod' te =
  let s1,s2,a,t = extract_prod te in
  let x,te =
    match t with
    | Lam(_,x,_,te) -> x,te
    | _ -> assert false
  in
  s1,s2,a,x,te

let rec elaborate_cuni sg te =
  if is_prop te then
    mk_prop, mk_cuni mk_prop
  else
    let v = fresh_uvar sg in
    v, mk_cuni v

and elaborate_prod sg ctx s1 s2 a x te =
  let s1',a', ctx' =
    if is_prop s1 then
      let a' = elaborate sg ctx a in
      s1,a',ctx
    else if is_cuni a then
      let v,a' =
        elaborate_cuni sg (extract_cuni a) in
      mk_succ v, a', (x,v)::ctx
    else if is_var a then
      let id = extract_var a in
      let s = elaborate_var sg ctx id in
      s, a, ctx
    else if is_prod a then
      let s1,s2,a,x,te = extract_prod' a in
      let s1',a' = elaborate_prod sg ctx s1 s2 a x te in
      s1',a', ctx
    else
      let a' = elaborate sg ctx a in
      if is_prop s1 then
        s1,a',ctx
      else
        fresh_uvar sg, a', ctx
  in
  let ty' = mk_term s1' a' in
  let s2',te' =
    if is_cuni te then
      let v, te' =
        elaborate_cuni sg (extract_cuni te) in
      mk_succ v, te'
    else if is_prod te then
      let s1,s2,a,x,te = extract_prod' te in
      elaborate_prod sg ctx' s1 s2 a x te
    else
      let te' =  elaborate sg ctx' te in
      if is_prop s2 then
        s2,te'
      else
        fresh_uvar sg, te'
  in
  mk_rule s1' s2',mk_prod s1' s2' a' x ty' te'

and elaborate_term sg ctx s1 t =
  let s1',t' =
    if is_prod t then
      let s1,s2,a,x,te = extract_prod' t in
      elaborate_prod sg ctx s1 s2 a x te
    else if is_var t then
      let id = extract_var t in
      elaborate_var sg ctx id, t
    else if is_prop s1 then
      s1, elaborate sg ctx t
    else
      fresh_uvar sg, elaborate sg ctx t
  in
  mk_term s1' t'

and elaborate_var sg ctx id =
  try
    List.assoc id ctx
  with _ -> fresh_uvar sg
and elaborate sg ctx t =
  if is_type t then
    fresh_uvar sg
  else if is_term t then
    let s1, t' = extract_term t in
    elaborate_term sg ctx s1 t'
  else if is_lift t then
    begin
      let _,_,t' = extract_lift t in
      if is_cuni t' then
        let s,t' = elaborate_cuni sg (extract_cuni t') in
        mk_lift (mk_succ s) (fresh_uvar sg) t'
      else
        mk_lift (fresh_uvar sg) (fresh_uvar sg) (elaborate sg ctx t')
    end
  else if is_prod t then
    let s1,s2,a,x,te = extract_prod' t in
    snd @@ elaborate_prod sg ctx s1 s2 a x te
  else
    match t with
    | App(f, a, al) ->
      let f' = elaborate sg ctx f in
      let a' = elaborate sg ctx a in
      let al' = List.map (elaborate sg ctx) al in
      mk_App f' a' al'
    | Lam(loc, id, Some ty, t) when is_univ ty ->
      let v = fresh_uvar sg in
      let ty' =  mk_univ v in
      let t' = elaborate sg ((id,v)::ctx) t in
      mk_Lam loc id (Some ty') t'
    | Lam(loc, id, t_opt, t) ->
      let ty' =
        match t_opt with
        | None -> None
        | Some ty -> Some (elaborate sg ctx ty)
      in
      let t' = elaborate sg ctx t in
      mk_Lam loc id ty' t'
    | Pi(loc, id, ta, tb) ->
      let ta' = elaborate sg ctx ta in
      let tb' = elaborate sg ctx tb in
      mk_Pi loc id ta' tb'
    | _ -> t

let elaboration sg e =
  let open Rule in
  let open Entry in
  match e with
  | Decl(l,id,st,t) ->
    Decl(l,id,st, elaborate sg [] t)
  | Def(l,id,op,pty,te) ->
    Def(l,id,op, Basic.map_opt (elaborate sg []) pty, elaborate sg [] te)
  | Rules(rs) ->
    let rs' = List.map (fun (r: untyped_rule) -> {r  with rhs = elaborate sg [] r.rhs}) rs in
    Rules(rs')
  | Name (l,id) -> Name(l,id)
  | _ -> assert false
