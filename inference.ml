open Types

let error_convertibility ?let_ctx te ctx exp inf =
  let exp = Reduction.whnf ?let_ctx exp in
  let inf = Reduction.whnf ?let_ctx inf in
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

let db_get_type l ctx n =
  try Subst.shift (n+1) (snd (List.nth ctx n))
  with Failure _ -> Global.fail l "Trying to type a open term."

let rec infer_rec ~let_ctx (ctx:context) (te:term)  : term =
  match te with
    | Kind -> Global.fail dloc "Kind is not typable."
    | Type _ -> mk_Kind
    | DB (l,_,n) -> db_get_type l ctx n
    | Const (l,md,id) -> Env.get_type l md id
    | App (f,a,args) ->
        snd (List.fold_left (infer_rec_aux ~let_ctx ctx) (f,infer_rec ~let_ctx ctx f) (a::args))
    | Pi (_,opt,a,b) ->
        let x = match opt with None -> empty | Some x -> x in
        let _ = is_type ctx a in
        let let_ctx = LList.cons None let_ctx in
        let ctx2 = (x,a)::ctx in
          ( match infer_rec ~let_ctx ctx2 b with
              | (Type _|Kind as tb) -> tb
              | ty_b -> error_not_a_sort b ctx2 ty_b )
    | Lam  (_,x,a,b) ->
        let _ = is_type ~let_ctx ctx a in
        let let_ctx = LList.cons None let_ctx in
        let ctx2 = (x,a)::ctx in
          ( match infer_rec ~let_ctx ctx2 b with
              | Kind -> error_kind b ctx2
              | ty   -> mk_Pi dloc (Some x) a ty )
    | Let (_,x,a,b) ->
        (* ctx |- let x=a in b   has type  tau
            iff ctx, x:ty_a |- tau  and ctx |- a : ty_a. *)
        let ty_a = infer_rec ~let_ctx ctx a in
        let let_ctx = LList.cons (Some a) let_ctx in
        infer_rec ~let_ctx ((x,ty_a)::ctx) b
    | Meta _ -> assert false

and infer_rec_aux ~let_ctx ctx (f,ty_f) u =
  match Reduction.whnf ~let_ctx ty_f , infer_rec ~let_ctx ctx u with
    | ( Pi (_,_,a1,b) , a2 ) ->
        if Reduction.are_convertible ~let_ctx a1 a2 then
          ( mk_App f u [] , Subst.subst b u )
        else error_convertibility u ctx a1 a2
    | ( _ , _ ) -> error_product f ctx ty_f

and is_type ~let_ctx ctx a =
  match infer_rec ~let_ctx ctx a with
    | Type _ -> ()
    | ty_a -> error_not_type a ctx ty_a

(******************************************************************************)

let underscore = hstring "_"

let rec t_of_p = function
  | Var (l,id,n) -> mk_DB l id n
  | Joker (l,n) -> mk_DB l underscore n
  | Brackets t -> t
  | Pattern (l,md,id,[]) -> mk_Const l md id
  | Pattern (l,md,id,a::args) ->
      mk_App (mk_Const l md id) (t_of_p a) (List.map t_of_p args)

let infer_pat ctx pat =
  let rec synth ~let_ctx = function
    | Var (l,x,n) -> db_get_type l ctx n
    | Brackets t -> infer_rec ~let_ctx ctx t
    | Pattern (l,md,id,args) ->
        snd (List.fold_left (check ~let_ctx)
          (mk_Const l md id,Env.get_type l md id) args)
    | Joker _ -> assert false
  and check ~let_ctx (f,ty_f) pat =
    match Reduction.whnf ~let_ctx ty_f, pat with
      | Pi (_,_,a1,b), Joker _ ->
          let u = t_of_p pat in ( mk_App f u [] , Subst.subst b u )
      | Pi (_,_,a1,b), _ ->
          let a2 = synth ~let_ctx pat in
          let u = t_of_p pat in
            if Reduction.are_convertible ~let_ctx a1 a2 then
              ( mk_App f u [] , Subst.subst b u )
            else error_convertibility ~let_ctx u ctx a1 a2
      | _, _ -> error_product f ctx ty_f
  in synth ~let_ctx:LList.nil pat

(******************************************************************************)

let infer pte =
  let te = Scoping.scope_term [] pte in
    ( te , infer_rec ~let_ctx:LList.nil [] te )

let check pte pty =
  let te = Scoping.scope_term [] pte in
  let ty = Scoping.scope_term [] pty in
  let let_ctx=LList.nil in
  let _  =  infer_rec ~let_ctx [] ty in
  let ty2 = infer_rec ~let_ctx [] te in
    if (Reduction.are_convertible ~let_ctx ty ty2) then (te,ty)
    else error_convertibility te [] ty ty2

let is_a_type2 ctx pty =
  let ty = Scoping.scope_term ctx pty in
    match infer_rec ~let_ctx:LList.nil ctx ty with
      | Type _ | Kind -> ty
      | s -> error_not_a_sort ty ctx s

let is_a_type = is_a_type2 []

let check_context =
  List.fold_left ( fun ctx (_,x,ty) -> (x,is_a_type2 ctx ty)::ctx ) []

let check_rule (l,pctx,id,pargs,pri) =
  let ctx = check_context pctx in
  let pat = Scoping.scope_pattern ctx (PPattern(l,None,id,pargs)) in
  let args = match pat with
    | Pattern (_,_,_,args) -> args
    | Var (l,_,_) -> Global.fail l "A pattern cannot be a variable."
    | _ -> assert false in
  let ty1 = infer_pat ctx pat in
  let rhs = Scoping.scope_term ctx pri in
  let ty2 = infer_rec ~let_ctx:LList.nil ctx rhs in
    if (Reduction.are_convertible ~let_ctx:LList.nil ty1 ty2) then
      { l=l ; ctx=ctx ; md= !Global.name; id=id ; args=args ; rhs=rhs }
    else error_convertibility rhs ctx ty1 ty2
