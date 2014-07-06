open Types

           (* ********************************************* *)

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

           (* ********************************************* *)

let db_get_type l ctx n =
  try Subst.shift (n+1) (snd (List.nth ctx n))
  with Failure _ -> Global.fail l "Trying to type a open term."

let rec infer_rec (ctx:context) (te:term)  : term =
  match te with
    | Kind -> Global.fail dloc "Kind is not typable."
    | Type _ -> mk_Kind
    | DB (l,_,n) -> db_get_type l ctx n
    | Const (l,md,id) -> Env.get_type l md id
    | App (f,a,args) ->
        snd (List.fold_left (infer_rec_aux ctx) (f,infer_rec ctx f) (a::args))
    | Pi (_,opt,a,b) ->
        let x = match opt with None -> empty | Some x -> x in
        let _ = is_type ctx a in
        let ctx2 = (x,a)::ctx in
          ( match infer_rec ctx2 b with
              | (Type _|Kind as tb) -> tb
              | ty_b -> error_not_a_sort b ctx2 ty_b )
    | Lam  (_,x,a,b) ->
        let _ = is_type ctx a in
        let ctx2 = (x,a)::ctx in
          ( match infer_rec ctx2 b with
              | Kind -> error_kind b ctx2
              | ty   -> mk_Pi dloc (Some x) a ty )
    | Meta _ -> assert false

and infer_rec_aux ctx (f,ty_f) u =
  match Reduction.whnf ty_f , infer_rec ctx u with
    | ( Pi (_,_,a1,b) , a2 ) ->
        if Reduction.are_convertible a1 a2 then
          ( mk_App f u [] , Subst.subst b u )
        else error_convertibility u ctx a1 a2
    | ( _ , _ ) -> error_product f ctx ty_f

and is_type ctx a =
  match infer_rec ctx a with
    | Type _ -> ()
    | ty_a -> error_not_type a ctx ty_a


(* ********************************************* *)

let infer pte =
  let te = Scoping.scope_term [] pte in
    ( te , infer_rec [] te )

let check pte pty =
  let te = Scoping.scope_term [] pte in
  let ty = Scoping.scope_term [] pty in
  let _  =  infer_rec [] ty in
  let ty2 = infer_rec [] te in
    if (Reduction.are_convertible ty ty2) then (te,ty)
    else error_convertibility te [] ty ty2

let is_a_type2 ctx pty =
  let ty = Scoping.scope_term ctx pty in
    match infer_rec ctx ty with
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
    | Brackets _ -> assert false in
  let ty1 = infer_rec ctx (term_of_pattern pat) in
  let rhs = Scoping.scope_term ctx pri in
  let ty2 = infer_rec ctx rhs in
    if (Reduction.are_convertible ty1 ty2) then
      { l=l ; ctx=ctx ; id=id ; args=args ; rhs=rhs }
    else error_convertibility rhs ctx ty1 ty2
