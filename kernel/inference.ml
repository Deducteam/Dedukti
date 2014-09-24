open Term
open Rule

let coc = ref false

let error_convertibility te ctx exp inf =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: %a\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term exp Pp.pp_term inf

let error_product te ctx inf =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a.\nExpected: a product type.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf

let error_product2 te ctx exp =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a. The expected type is not a product.\nExpected: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term exp
(*
let error_product_pat pat ctx inf =
  Print.fail (get_loc_pat pat)
    "Error while typing '%a' in context:\n%a.\nExpected: a product type.\nInferred: %a."
      Pp.pp_pattern pat Pp.pp_context ctx Pp.pp_term inf
 *)
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
(*
let error_joker te =
  Print.fail (get_loc te)
    "Error while typing '%a'. The type should not contain jokers." Pp.pp_term te
 *)
(******************************************************************************)

let db_get_type l ctx n =
  try Subst.shift (n+1) (snd (List.nth ctx n))
  with Failure _ -> Print.fail l "Trying to type a open term."

let rec infer (ctx:context) : term -> term = function
    | Kind -> Print.fail dloc "Kind is not typable."
    | Type _ -> mk_Kind
    | DB (l,_,n) -> db_get_type l ctx n
    | Const (l,md,id) -> Env.get_type l md id
    | App (f,a,args) ->
        let ty_f = infer ctx f in
          snd (List.fold_left (check_app ctx) (f,ty_f) (a::args))
    | Pi (_,x,a,b) ->
        begin
          let _ = check_annot ctx a in
          let ctx2 = (x,a)::ctx in
            ( match infer ctx2 b with
                | (Type _|Kind as tb) -> tb
                | ty_b -> error_not_a_sort b ctx2 ty_b )
        end
    | Lam  (_,x,Some a,b) ->
        begin
          let _ = check_annot ctx a in
          let ctx2 = (x,a)::ctx in
            ( match infer ctx2 b with
                | Kind -> error_kind b ctx2
                | ty   -> mk_Pi dloc x a ty )
        end
    | Lam  (l,x,None,b) ->
        Print.fail l "Cannot infer the type of domain-free lambda."

and check_annot ctx a =
  match infer ctx a with
    | Type _ -> ()
    | Kind -> if not !coc then
        error_convertibility a ctx (mk_Type dloc) mk_Kind
    | ty_a ->  error_not_a_sort a ctx ty_a

and check (ctx:context) (te:term) (ty_exp:term) : unit =
    match te with
      | Lam (_,x,None,u) ->
          begin
            match Reduction.whnf ty_exp with
              | Pi (_,x,a1,b) -> check ((x,a1)::ctx) u b
              | _ -> error_product2 te ctx ty_exp
          end
      | _ -> let ty_inf = infer ctx te in
          if not (Reduction.are_convertible ty_exp ty_inf) then
            error_convertibility te ctx ty_exp ty_inf

  and check_app (ctx:context) (f,ty_f:term*term) (arg:term) : term*term =
    match Reduction.whnf ty_f with
    | Pi (_,_,a,b) -> ( check ctx arg a; ( mk_App f arg [] , Subst.subst b arg ) )
    | _ -> error_product f ctx ty_f

let is_a_type ctx ty =
  match infer ctx ty with
    | Type _ | Kind -> ()
    | s -> error_not_a_sort ty ctx s

(******************************************************************************)

let check_context (ctx:context) : unit =
  let aux ctx0 a = is_a_type ctx0 (snd a); a::ctx0
  in ignore (List.fold_left aux [] (List.rev ctx))

let check_rule (r0:rule) : unit =
  let r = Underscore.refine_rule r0 in
  let lhs = pattern_to_term (Pattern(r.l,r.md,r.id,r.args)) in
  let _ = check_context r.ctx in
  let ty = infer r.ctx lhs in
    check r.ctx r.rhs ty

(******************************************************************************)

let infer2 pte =
  let te = Scoping.scope_term [] pte in
    ( te , infer [] te )

let check2 pte pty =
  let te = Scoping.scope_term [] pte in
  let ty = Scoping.scope_term [] pty in
  let _  =  is_a_type [] ty in
  let _  = check [] te ty in
    ( te , ty )

let is_a_type2 pty =
  let ty = Scoping.scope_term [] pty in
  is_a_type [] ty; ty

let check_prule pr =
  let r = Scoping.scope_rule pr in
  check_rule r; r
