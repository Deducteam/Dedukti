open Basic
open Term
open Cic
open Uvar

(* TODO: remove the var case rule *)
type binding = { ty : Term.term ; sort : Term.term }

let extract_prod' te =
  let s1,s2,a,t = extract_prod te in
  let x,te =
    match t with
    | Lam(_,x,_,te) -> x,te
    | _ -> assert false
  in
  s1,s2,a,x,te

let mk_type0 = mk_type mk_z

(* prop will is already minimal *)
let elaborate_sort sg sort =
  if is_prop sort then
    mk_prop
  else
    fresh_uvar sg

let elaborate_cuni sg s =
  let s = elaborate_sort sg s in
  mk_succ s, mk_cuni s

let elaborate_var sg ctx var =
  Format.eprintf "debug: %a@." Pp.print_term var;
  let id = extract_var var in
  if List.mem_assoc id ctx then
    if is_cuni (List.assoc id ctx).ty then
      extract_cuni (List.assoc id ctx).ty, var
    else
    (List.assoc id ctx).sort, var
  else (* inside rule *)
    fresh_uvar sg, var

let rec elaborate_prod sg ctx s1 s2 a x b =
  let s1',a' = elaborate sg ctx a in
  let ctx' = (x,{ty=a';sort=s1'})::ctx in
  let s2',b' = elaborate sg ctx' b in
  let ty' = mk_term s1' a' in
  mk_rule s1' s2', mk_prod s1' s2' a' x ty' b'

and elaborate_cast sg ctx s1 s2 a b t =
  let s1',a' =
    if is_var t then
      try
        (List.assoc (extract_var t) ctx).sort, (List.assoc (extract_var t) ctx).ty
      with _ -> elaborate sg ctx a
    else
      elaborate sg ctx a
  in
  let s2',b' = elaborate sg ctx b in
  let s,t'  = elaborate sg ctx t in
  mk_max s1' s2', mk_cast s1' s2' a' b' t'

and elaborate sg ctx t =
  if is_cuni t then
    let s = extract_cuni t in
    elaborate_cuni sg s
  else if is_prod t then
    let s1,s2,a,x,b = extract_prod' t in
    elaborate_prod sg ctx s1 s2 a x b
  else if is_var t then
    elaborate_var sg ctx t
  else if is_cast t then
    let s1,s2,a,b,t = extract_cast t in
    elaborate_cast sg ctx s1 s2 a b t
  else
    match t with
    | App(f, a, al) ->
      let s,f' = elaborate sg ctx f in
      let _,a' = elaborate sg ctx a in
      let _,al' = List.split (List.map (elaborate sg ctx) al) in
      s, mk_App f' a' al'
    | Lam(loc, id, Some ty, t) when is_univ ty ->
      let s',u', ty' = elaborate_term sg ctx ty in
      let ctx' = ((id,{ty=u';sort=s'})::ctx) in
      let st,t' = elaborate sg ctx' t in
      st,mk_Lam loc id (Some ty') t'
    | Lam(loc, id, None, t) -> failwith "untyped lambdas are not supported"
    | Pi(loc, id, ta, tb) -> assert false
    | _ -> fresh_uvar sg, t


and elaborate_term sg ctx t =
  if is_term t then
    let s,t   = extract_term t in
    let s',t' = elaborate sg [] t in
    s',t',mk_term s' t'
  else if is_univ t then
    let s = extract_univ t in
    if is_prop s then
      mk_prop, mk_cuni mk_prop, t
    else
      let s = fresh_uvar sg in
      s, mk_cuni s, mk_univ (fresh_uvar sg)
  else
    assert false

(*
let rec elaborate_prod sg ctx s1 s2 a x te =
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
  else if is_cast t then
    begin
      let _,_,tyl,tyr,t' = extract_cast t in
      if is_cuni tyl then
        begin
          assert (is_cuni tyr);
          assert (is_var t');
          let s,tyr' = elaborate_cuni sg (extract_cuni tyr) in
          let var = extract_var t' in
          let svar = List.assoc var ctx in
          mk_cast (mk_succ svar) (mk_succ s) (mk_cuni svar) tyr' t'
        end
      else if is_prod tyl then
        begin
          assert (is_prod tyr);
          failwith "todo"
        end
      else assert false
        (* mk_cast (fresh_uvar sg) (fresh_uvar sg) (elaborate sg ctx t') *)
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
    | Pi(loc, id, ta, tb) -> assert false
    | _ -> t
           *)

let elaboration sg e =
  let open Rule in
  let open Entry in
  match e with
  | Decl(l,id,st,t) ->
    let _, _, t' = elaborate_term sg [] t in
    Decl(l,id,st, t')
  | Def(l,id,op,pty,te) -> (
    match pty with
    | None ->
      Def(l,id,op, None, snd @@ elaborate sg [] te)
    | Some ty ->
      let _,_,ty'    = elaborate_term sg [] ty in
      let _, te' = elaborate sg [] te in
    Def(l,id,op, Some ty', te'))
  | Rules(rs) ->
    let rs' = List.map
        (fun (r: untyped_rule) -> {r  with rhs = snd @@ elaborate sg [] r.rhs}) rs in
    Rules(rs')
  | Name (l,id) -> Name(l,id)
  | _ -> assert false
