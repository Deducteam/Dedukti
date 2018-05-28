open Basic
open Term

module Dk =
struct
  let cic = mk_mident "cic"

  let mk_const id = mk_Const dloc (mk_name cic id)

  let z = mk_name cic (mk_ident "z")

  let s = mk_name cic (mk_ident "s")

  let succ = mk_name cic (mk_ident "succ")

  let sort = mk_name cic (mk_ident "Sort")

  let cast = mk_name cic (mk_ident "cast")

  let max = mk_name cic (mk_ident "max")

  let rule = mk_name cic (mk_ident "rule")

  let prop = mk_name cic (mk_ident "prop")

  let typ = mk_name cic (mk_ident "type")

  let univ = mk_name cic (mk_ident "Univ")

  let cuni = mk_name cic (mk_ident "univ")

  let term = mk_name cic (mk_ident "Term")

  let prod = mk_name cic (mk_ident "prod")

  let is_const cst t =
    match t with
    | Const(_,n) -> name_eq cst n
    | _ -> false

  let is_z t =
    match t with
    | Const(_,u) when is_const z t -> true
    | _ -> false

  let is_s t =
    match t with
    | App(u,_,[]) when is_const s u -> true
    | _ -> false

  let is_term t =
    match t with
    | App(u,_,[_]) when is_const term u -> true
    | _ -> false

  let is_univ t =
    match t with
    | App(u,_,[]) when is_const univ u -> true
    | _ -> false

  let is_cuni t =
    match t with
    | App(u,_,[]) when is_const cuni u -> true
    | _ -> false

  let is_prop t =
    match t with
    | Const(_,n) when is_const prop t -> true
    | _ -> false

  let is_type t =
    match t with
    | App(t,_,[]) when is_const typ t -> true
    | _ -> false

  let is_succ t =
    match t with
    | App(c,arg,[]) when is_const succ c -> true
    | _ -> false

  let is_cast t =
    match t with
    | App(c, s1, [s2;t1;t2;a]) when is_const cast c -> true
    | _ -> false

  let is_max t =
    match t with
    | App(c, s1, [s2]) when is_const max c -> true
    | _ -> false

  let is_rule t =
    match t with
    | App(c, s1, [s2]) when is_const rule c -> true
    | _ -> false

  let is_prod t =
    match t with
    | App(c, s1, [s2;a;f]) when is_const prod c -> true
    | _ -> false

  let is_var t =
    match t with
    | DB _ -> true
    | _ -> false

  let is_lam t =
    match t with
    | Lam _ -> true
    | _ -> false

  let is_app t =
    match t with
    | App _ -> true
    | _ -> false

  let extract_app t =
    match t with
    | App (f,a,args) -> f,a,args
    | _ -> failwith "is not an app"

  let extract_var t =
    match t with
    | DB(_,id,_) -> id
    | _ -> failwith "is not a local variable"

  let extract_s t =
    match t with
    | App(t,u,[]) when is_const s t -> u
    | _ -> failwith "is not a s"

  let extract_type t =
    match t with
    | App(t,u,[]) when is_const typ t -> u
    | _ -> failwith "is not a type"

  let extract_term t =
    match t with
    | App(t,s,[u]) when is_const term t -> s,u
    | _ -> failwith "is not a term"

  let extract_succ t =
    match t with
    | App(c,arg,[]) when is_const succ c -> arg
    | _ -> failwith "is not a succ"

  let extract_cast t =
    match t with
    | App(c,s1,[s2;t1;t2;a]) when is_const cast c -> s1,s2,t1,t2,a
    | _ -> failwith "is not a cast"

  let extract_max t =
    match t with
    | App(c,s1,[s2]) when is_const max c -> s1,s2
    | _ -> failwith "is not a max"

  let extract_rule t =
    match t with
    | App(c, s1, [s2]) when is_const rule c -> s1, s2
    | _ -> failwith "is not a rule"

  let extract_univ t =
    match t with
    | App(c, s, []) when is_const univ c -> s
    | _ -> failwith "is not a univ"

  let extract_cuni t =
    match t with
    | App(c, s, []) when is_const cuni c -> s
    | _ -> failwith "is not a cuni"

  let extract_prod t =
    match t with
    | App(c, s1, [s2;a;f]) when is_const prod c -> s1,s2,a,f
    | _ -> failwith "is not a prod"

  let extract_lam t =
    match t with
    | Lam(_,x,Some ty,te) -> x,ty,te
    | _ -> failwith "not a lambda or lambda without a type"

  let extract_succ t =
    match t with
    | App(c,arg,[]) when is_const succ c -> arg
    | _ -> failwith "is not a succ"

  let mk_prop     = mk_Const dloc prop

  let mk_z        = mk_Const dloc z

  let mk_s arg    = mk_App (mk_Const dloc s) arg []

  let mk_type arg = mk_App (mk_Const dloc typ) arg []

  let mk_succ arg = mk_App (mk_Const dloc succ) arg []

  let mk_rule left right  = mk_App (mk_Const dloc rule) left [right]

  let mk_max  left right  = mk_App (mk_Const dloc max) left [right]

  let mk_univ s =
    mk_App (mk_Const dloc univ) s []

  let mk_cuni s =
    mk_App (mk_Const dloc cuni) s []

  let mk_cast s1 s2 t1 t2 a =
    mk_App (mk_Const dloc cast) s1 [s2;t1;t2;a]

  let mk_prod s1 s2 a x ty te =
    mk_App (mk_Const dloc prod) s1 [s2;a;(mk_Lam dloc x (Some ty) te)]

  let mk_term s a =
    mk_App (mk_Const dloc term) s [a]

  let assert_type_zero t =
    if is_z (extract_type t) then
      ()
    else
      failwith "This bug should be reported (assert_type_zero)"

  let mk_sort = mk_Const dloc sort
end

module Elaboration =
struct
  open Dk

  type binding = { ty : Term.term ; sort : Term.term }

  let name : name ref = ref (mk_name (mk_mident "") (mk_ident ""))

  let add_name name =
    Cfg.add_name name;
    name

  let vars = ref []

  let add_vars () =
    let vars = ISet.of_list !vars in
    Cfg.add_uvars !name vars

  let fresh_uvar sg =
    let var = Uvar.fresh_uvar sg in
    let v = Uvar.name_of_uvar var in
    vars := (id v)::!vars;
    var


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
    (* Format.eprintf "debug:%d@.%a@." (List.length ctx) Pp.print_term var; *)
    let id = extract_var var in
    if List.mem_assoc id ctx then
      if is_cuni (List.assoc id ctx).ty then
        extract_cuni (List.assoc id ctx).ty, var
      else
        (List.assoc id ctx).sort, var
    else assert false

  let if_prop s = if Cic.is_prop s then true else false

  let rec elaborate_prod sg ctx s1 s2 a x b =
    let s1',a' = elaborate sg ctx (if_prop s1) a in
    let ctx' = (x,{ty=a';sort=s1'})::ctx in
    let s2',b' = elaborate sg ctx' (if_prop s2) b in
    let ty' = mk_term s1' a' in
    let srule = if is_prop s2' then s2' else mk_rule s1' s2' in
    srule, mk_prod s1' s2' a' x ty' b'

  and elaborate_cast sg ctx s1 s2 a b t =
    let s1',a' =
      if is_var t then
        try
          (List.assoc (extract_var t) ctx).sort, (List.assoc (extract_var t) ctx).ty
        with _ -> elaborate sg ctx (if_prop s1) a
      else
        elaborate sg ctx (if_prop s1) a
    in
    let s2',b' = elaborate sg ctx (if_prop s2) b in
    let s,t'  = elaborate sg ctx false t in
    mk_max s1' s2', mk_cast s1' s2' a' b' t'

  and elaborate sg ctx is_prop t =
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
        let _,f' = elaborate sg ctx is_prop f in
        let _,a' = elaborate sg ctx is_prop a in
        let _,al' = List.split (List.map (elaborate sg ctx is_prop) al) in
        if is_prop then
          mk_prop, mk_App f' a' al'
        else
          fresh_uvar sg, mk_App f' a' al'
      | Lam(loc, id, Some ty, t) ->
        let s',u', ty' = elaborate_term sg ctx ty in
        let ctx' = ((id,{ty=u';sort=s'})::ctx) in
        let st,t' = elaborate sg ctx' is_prop t in
        if is_prop then
          mk_prop, mk_Lam loc id (Some ty') t'
        else
          fresh_uvar sg,mk_Lam loc id (Some ty') t'
      | Lam(loc, id, None, t) -> failwith "untyped lambdas are not supported"
      | Pi(loc, id, ta, tb) -> assert false
      | _ -> if is_prop then mk_prop,t else fresh_uvar sg, t


  and elaborate_term sg ctx t =
    if is_term t then
      let s,t   = extract_term t in
      let s',t' = elaborate sg ctx (if_prop s) t in
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

  let forget_types : typed_context -> untyped_context =
    fun ctx -> List.map (fun (lc,id,_) -> (lc,id)) ctx

  let ctx_of_rule_ctx sg ctx =
    let add_binding ctx (l,x,t) =
      let s',u',_ = elaborate_term sg ctx t in
      ((x,{ty=u'; sort=s'})::ctx)
    in
    List.fold_left add_binding []  (List.rev ctx)

  let get_rule_name (r:'a Rule.rule) =
    let open Rule in
    match r.name with
    | Gamma(_,name) -> name
    | _ -> assert false

  let rule_elaboration sg (r:Rule.typed_rule) =
    let open Rule in
    name := get_rule_name r;
    let _,rhs' = elaborate sg (ctx_of_rule_ctx sg r.ctx) false r.rhs in
    let ctx' = forget_types r.ctx in
    add_vars ();
    {r with rhs=rhs'; ctx=ctx'}


  let elaboration md e =
    let open Rule in
    let open Entry in
    let sg = Cfg.get_signature () in
    vars := [];
    match e with
    | Decl(l,id,st,t) ->
      (* Format.eprintf "%a@." Pp.print_ident id; *)
      name := add_name (mk_name md id);
      let _, _, t' = elaborate_term sg [] t in
      add_vars();
      Decl(l,id,st, t')
    | Def(l,id,op,pty,te) -> (
        (*      Format.eprintf "%a@." Pp.print_ident id; *)
        name := add_name (mk_name md id);
        match pty with
        | None ->
          add_vars ();
          Def(l,id,op, None, snd @@ elaborate sg [] false te)
        | Some ty ->
          let s,_,ty'    = elaborate_term sg [] ty in
          let _, te' = elaborate sg [] (if_prop s) te in
          add_vars ();
          Def(l,id,op, Some ty', te'))
    | Rules(rs) ->
      let rs2 = List.map (Typing.check_rule sg)  rs in
      let rs' = List.map (rule_elaboration sg) rs2 in
      Rules(rs')
    | Name (l,id) -> Name(l,id)
    | _ -> assert false
end

module Reconstruction =
struct
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
end

include Elaboration

include Reconstruction

let univ_convertible foo = failwith "todo"
