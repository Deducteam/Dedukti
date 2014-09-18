open Term
open Rule

let error_convertibility te ctx exp inf =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: %a\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term exp Pp.pp_term inf

let error_product te ctx inf =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: a product type.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf

let error_product_pat pat ctx inf =
  Print.fail (get_loc_pat pat)
    "Error while typing '%a' in context:\n%a.\nExpected: a product type.\nInferred: %a."
      Pp.pp_pattern pat Pp.pp_context ctx Pp.pp_term inf

let error_not_a_sort te ctx inf =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: Type or Kind.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf

let error_kind te ctx =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: anything but Kind.\nInferred: Kind."
      Pp.pp_term te Pp.pp_context ctx

let error_not_type te ctx inf =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: Type.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf

(******************************************************************************)

let db_get_type l ctx n =
  try Subst.shift (n+1) (snd (List.nth ctx n))
  with Failure _ -> Print.fail l "Trying to type a open term."

let rec infer (ctx:context) = function
  | Kind -> Print.fail dloc "Kind is not typable."
  | Type _ -> mk_Kind
  | DB (l,_,n) -> db_get_type l ctx n
  | Const (l,md,id) -> Env.get_type l md id
  | App (f,a,args) ->
      snd (List.fold_left (infer_app ctx) (f,infer ctx f) (a::args))
    | Pi (_,x,a,b) ->
        let _ = is_type ctx a in
        let ctx2 = (x,a)::ctx in
          ( match infer ctx2 b with
              | (Type _|Kind as tb) -> tb
              | ty_b -> error_not_a_sort b ctx2 ty_b )
    | Lam  (_,x,a,b) ->
        let _ = is_type ctx a in
        let ctx2 = (x,a)::ctx in
          ( match infer ctx2 b with
              | Kind -> error_kind b ctx2
              | ty   -> mk_Pi dloc x a ty )

and infer_app ctx (f,ty_f) u =
  match Reduction.whnf ty_f , infer ctx u with
    | ( Pi (_,_,a1,b) , a2 ) ->
        if Reduction.are_convertible a1 a2 then
          ( mk_App f u [] , Subst.subst b u )
        else error_convertibility u ctx a1 a2
    | ( _ , _ ) -> error_product f ctx ty_f

and is_type ctx a =
  match infer ctx a with
    | Type _ -> ()
    | ty_a -> error_not_type a ctx ty_a

let is_a_type ctx ty =
  match infer ctx ty with
    | Type _ | Kind -> ()
    | s -> error_not_a_sort ty ctx s

let check ctx te ty_exp =
  let ty_inf = infer ctx te in
    if not (Reduction.are_convertible ty_exp ty_inf) then
      error_convertibility te ctx ty_exp ty_inf

(******************************************************************************)

let infer_pat (ctx:context) (pat:pattern) : term (*the type*) =

  let rec synth (ctx:context) : pattern -> term*term = function
    | MatchingVar (l,x,n,args) ->
        let args2 = List.map (fun (l,id,n) -> BoundVar (l,id,n,[])) args in
          List.fold_left (check_app ctx) (mk_DB l x n,db_get_type l ctx n) args2
    | BoundVar (l,x,n,args) ->
        List.fold_left (check_app ctx) (mk_DB l x n,db_get_type l ctx n) args
    | Pattern (l,md,id,args) ->
        List.fold_left (check_app ctx) (mk_Const l md id,Env.get_type l md id) args
    | Brackets t -> ( t , infer ctx t )
    | Lambda (_,_,_) -> assert false
    | Joker _ -> assert false

  and check (ctx:context) (ty:term) : pattern -> term = function
      | Joker l -> (*mk_Joker l TODO*) failwith "Not implemented."
      | Lambda (l,x,pat2) as f ->
          ( match Reduction.whnf ty with
              | Pi (_,x,a1,b) ->
                  let u = check ((x,a1)::ctx) b pat2 in
                    mk_Lam l x a1 u
              | _ -> error_product_pat f ctx ty )
      | pat ->
          let (u,ty2) = synth ctx pat in
            if Reduction.are_convertible ty ty2 then u
            else error_convertibility u ctx ty ty2

  and check_app (ctx:context) (f,ty_f:term*term) (pat:pattern) : term*term =
    match Reduction.whnf ty_f, pat with
      | Pi (_,_,a,b), _ ->
          let u = check ctx a pat in
            ( mk_App f u [] , Subst.subst b u )
      | _, _ -> error_product f ctx ty_f

  in snd (synth ctx pat)

(******************************************************************************)

let check_context (ctx:context) : unit =
  let aux ctx0 a = is_a_type ctx0 (snd a); a::ctx0
  in ignore (List.fold_left aux [] ctx)

let check_rule r =
  let _ = check_context r.ctx in
  let ty = infer_pat r.ctx (Pattern(r.l,Env.get_name (),r.id,r.args)) in
    check r.ctx r.rhs ty

(******************************************************************************)

let infer_pat (ctx:context) (pat:pattern) : term (*the type*) =

  let rec synth (ctx:context) : pattern -> term*term = function
    | MatchingVar (l,x,n,args) ->
        let args2 = List.map (fun (l,id,n) -> BoundVar (l,id,n,[])) args in
          List.fold_left (check_app ctx) (mk_DB l x n,db_get_type l ctx n) args2
    | BoundVar (l,x,n,args) ->
        List.fold_left (check_app ctx) (mk_DB l x n,db_get_type l ctx n) args
    | Pattern (l,md,id,args) ->
        List.fold_left (check_app ctx) (mk_Const l md id,Env.get_type l md id) args
    | Brackets t -> ( t , infer ctx t )
    | Lambda (_,_,_) -> assert false
    | Joker _ -> assert false

  and check (ctx:context) (ty:term) : pattern -> term = function
      | Joker l -> (*mk_Joker l TODO*) failwith "Not implemented."
      | Lambda (l,x,pat2) as f ->
          ( match Reduction.whnf ty with
              | Pi (_,x,a1,b) ->
                  let u = check ((x,a1)::ctx) b pat2 in
                    mk_Lam l x a1 u
              | _ -> error_product_pat f ctx ty )
      | pat ->
          let (u,ty2) = synth ctx pat in
            if Reduction.are_convertible ty ty2 then u
            else error_convertibility u ctx ty ty2

  and check_app (ctx:context) (f,ty_f:term*term) (pat:pattern) : term*term =
    match Reduction.whnf ty_f, pat with
      | Pi (_,_,a,b), _ ->
          let u = check ctx a pat in
            ( mk_App f u [] , Subst.subst b u )
      | _, _ -> error_product f ctx ty_f

  in snd (synth ctx pat)

(******************************************************************************)

let infer2 pte =
  let te = Scoping.scope_term [] pte in
    ( te , infer [] te )

let check2 pte pty =
  let te = Scoping.scope_term [] pte in
  let ty = Scoping.scope_term [] pty in
  let _  =  is_a_type [] ty in
  let ty2 = infer [] te in
    if (Reduction.are_convertible ty ty2) then (te,ty)
    else error_convertibility te [] ty ty2

let is_a_type2 pty =
  let ty = Scoping.scope_term [] pty in
  is_a_type [] ty; ty

let check_prule pr =
  let r = Scoping.scope_rule pr in
  check_rule r; r
