open Types

let error_convertibility te ctx exp inf =
  Global.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: %a\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term exp Pp.pp_term inf

let error_product te ctx inf =
  Global.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: a product type.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf

let error_not_a_sort te ctx inf =
  Global.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: Type or Kind.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf

let error_kind te ctx =
  Global.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: anything but Kind.\nInferred: Kind."
      Pp.pp_term te Pp.pp_context ctx

let error_not_type te ctx inf =
  Global.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: Type.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf

(******************************************************************************)

let var_get_type l ctx v =
  try VarMap.find v ctx
  with Not_found -> Global.fail l "Trying to type a open term (var %a)." Var.pp v

let tau = hstring "τ"

(* ctx: maps variables to their type *)
let rec infer_rec ctx (te:term) : term =
  match te with
    | Kind -> Global.fail dloc "Kind is not typable."
    | Type _ -> mk_Kind
    | Var (l, v) -> var_get_type l ctx.var2ty v
    | Const (l,md,id) -> Env.get_type l md id
    | App (f,a,args) ->
        snd (List.fold_left (infer_rec_aux ctx) (f,infer_rec ctx f) (a::args))
    | Pi (_,opt,a,b) ->
        let x = match opt with
          | None -> Var.fresh_of_ident tau
          | Some x -> x
        in
        let _ = is_type ctx a in
        let ctx2 = ctx_bind ctx x a in
        begin match infer_rec ctx2 b with
          | (Type _|Kind as tb) -> tb
          | ty_b -> error_not_a_sort b ctx2 ty_b
        end
    | Lam (_,x,a,b) ->
        let _ = is_type ctx a in
        let ctx2 = ctx_bind ctx x a in
        begin match infer_rec ctx2 b with
          | Kind -> error_kind b ctx2
          | ty   -> mk_Pi dloc (Some x) a ty
        end
    | Let (_,x,a,b) ->
        (* ctx |- let x=a in b   has type  tau
            iff ctx, x:ty_a |- tau  and ctx |- a : ty_a *)
        let ty_a = infer_rec ctx a in
        let ctx' = ctx_bind ctx x ty_a in
        infer_rec ctx' b
    | Meta _ -> assert false

(* infer the type of [f u], where [ty_f] is the current type of [f] *)
and infer_rec_aux ctx (f,ty_f) u =
  match Reduction.whnf ty_f, infer_rec ctx u with
    | Pi (_,v_opt,a,b), ty_u ->
        if Reduction.are_convertible a ty_u
        then
          let ty = match v_opt with
            | None -> b
            | Some var -> Subst.subst b ~var ~by:u
          in
          mk_App f u [], ty
        else error_convertibility u ctx a ty_u
    | ( _ , _ ) -> error_product f ctx ty_f

and is_type ctx a =
  match infer_rec ctx a with
    | Type _ -> ()
    | ty_a -> error_not_type a ctx ty_a

(******************************************************************************)

let underscore = hstring "_"

let rec t_of_p = function
  | Var (l,v) -> mk_Var l v
  | Joker (l,_n) -> mk_Var l (Var.fresh_of_ident underscore)
  | Brackets t -> t
  | Pattern (l,md,id,[]) -> mk_Const l md id
  | Pattern (l,md,id,a::args) ->
      mk_App (mk_Const l md id) (t_of_p a) (List.map t_of_p args)

let infer_pat ctx pat =
  let rec synth = function
    | Var (l,v) -> var_get_type l ctx.var2ty v
    | Brackets t -> infer_rec ctx t
    | Pattern (l,md,id,args) ->
        snd (List.fold_left check (mk_Const l md id,Env.get_type l md id) args)
    | Joker _ -> assert false
  and check (f,ty_f) pat =
    match Reduction.whnf ty_f, pat with
      | Pi (_,v_opt,a1,b), Joker _ ->
          let u = t_of_p pat in
          let ty = match v_opt with
            | None -> b
            | Some var -> Subst.subst b ~var ~by:u
          in
          mk_App f u [], ty
      | Pi (_,v_opt,a1,b), _ ->
          let u = t_of_p pat in
          let a2 = synth pat in
          if Reduction.are_convertible a1 a2
          then
            let ty = match v_opt with
              | None -> b
              | Some var -> Subst.subst b ~var ~by:u
            in
            mk_App f u [], ty
          else error_convertibility u ctx a1 a2
      | _, _ -> error_product f ctx ty_f
  in synth pat

(******************************************************************************)

let infer pte =
  let te = Scoping.scope_term ~ctx:ctx_empty pte in
  te, infer_rec ctx_empty te

let check pte pty =
  let te = Scoping.scope_term ~ctx:ctx_empty pte in
  let ty = Scoping.scope_term ~ctx:ctx_empty pty in
  let _  =  infer_rec ctx_empty ty in
  let ty2 = infer_rec ctx_empty te in
  if (Reduction.are_convertible ty ty2)
  then (te,ty)
  else error_convertibility te ctx_empty ty ty2

let is_a_type2 ctx pty =
  let ty = Scoping.scope_term ~ctx pty in
  match infer_rec ctx ty with
    | Type _ | Kind -> ty
    | s -> error_not_a_sort ty ctx s

let is_a_type ty = is_a_type2 ctx_empty ty

let check_context l =
  let ctx = List.fold_left
    (fun ctx (_,x,p_ty) ->
      let ty = is_a_type2 ctx p_ty in
      (* bind both x to a variable, and the variable to its type *)
      let v = Var.fresh_of_ident x in
      let ctx = ctx_bind ctx v ty in
      ctx_bind_ident ctx x v
    ) ctx_empty l
  in ctx

let check_rule ((l,pctx,id,pargs,pri):prule) =
  let ctx = check_context pctx in
  let pat = Scoping.scope_pattern ~ctx (PPattern(l,None,id,pargs)) in
  let args = match pat with
    | Pattern (_,_,_,args) -> args
    | Var (l,_) -> Global.fail l "A pattern cannot be a variable."
    | _ -> assert false in
  let ty1 = infer_pat ctx pat in
  let rhs = Scoping.scope_term ~ctx pri in
  let ty2 = infer_rec ctx rhs in
  if (Reduction.are_convertible ty1 ty2)
    then { l=l ; ctx=ctx ; md= !Global.name; id=id ; args=args ; rhs=rhs }
    else error_convertibility rhs ctx ty1 ty2
