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

let error_product2 te ctx exp =
  Print.fail (get_loc te)
    "Error while typing '%a' in context:\n%a. The expected type is not a product.\nExpected: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term exp

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

let error_joker te =
  Print.fail (get_loc te)
    "Error while typing '%a'. The type should not contain jokers." Pp.pp_term te

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
        let _ = check ctx a (mk_Type dloc) in
        let ctx2 = (x,a)::ctx in
          ( match infer ctx2 b with
              | (Type _|Kind as tb) -> tb
              | ty_b -> error_not_a_sort b ctx2 ty_b )
    | Lam  (_,x,Some a,b) ->
        let _ = check ctx a (mk_Type dloc) in
        let ctx2 = (x,a)::ctx in
          ( match infer ctx2 b with
              | Kind -> error_kind b ctx2
              | ty   -> mk_Pi dloc x a ty )
    | Lam  (l,x,None,b) ->
        Print.fail l "Cannot infer the type of domain-free lambda."

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

let joker l = mk_Const l (Env.get_name ()) qmark

type term_with_joker = bool*term
(*the boolean should be true when the term contains a joker (underscore) *)

exception SubstCheck
(* Performs the subsitution but check that u is never actually substitute *)
let subst_check (te:term) (u:term) =
  let rec  aux k = function
    | DB (l,x,n) as t ->
        if n = k then raise SubstCheck
        else if n>k then mk_DB l x (n-1)
        else (*n<k*) t
    | Type _ | Kind | Const _ as t -> t
    | Lam (_,x,_,b) -> mk_Lam dloc x None (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi dloc  x (aux k a) (aux(k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te

let infer_pat (ctx:context) (pat:pattern) : term (*the type*) =
  (* Return the pattern as a term and its type *)
  let rec synth (ctx:context) : pattern -> term_with_joker*term = function
    | MatchingVar (l,x,n,args) ->
        let args2 = List.map (fun (l,id,n) -> BoundVar (l,id,n,[])) args in
          List.fold_left (check_app ctx)
            ( (false,mk_DB l x n) , db_get_type l ctx n ) args2
    | BoundVar (l,x,n,args) ->
        List.fold_left (check_app ctx)
          ( (false,mk_DB l x n) , db_get_type l ctx n ) args
    | Pattern (l,md,id,args) ->
        List.fold_left (check_app ctx)
          ( (false,mk_Const l md id) , Env.get_type l md id ) args
    | Brackets t -> ( (false,t) , infer ctx t )
    | Lambda (_,_,_) -> assert false
    | Joker _ -> assert false
  (* Return the pattern as a term *)
  and check (ctx:context) (ty:term) : pattern -> term_with_joker  = function
      | Joker l -> ( true, joker l )
      | Lambda (l,x,pat2) as f ->
          begin
            match Reduction.whnf ty with
              | Pi (_,x,a1,b) ->
                  let (bo,u) = check ((x,a1)::ctx) b pat2 in
                    ( bo , mk_Lam l x None u )
              | _ -> error_product_pat f ctx ty
          end
      | pat ->
          let (u,ty2) = synth ctx pat in
            if Reduction.are_convertible ty ty2 then u
            else error_convertibility (snd u) ctx ty ty2
  (* Return the application of (App [f] [pat]) as a term and its type *)
  and check_app (ctx:context) ((bo1,f),ty_f:term_with_joker*term) (pat:pattern) : term_with_joker*term =
    match Reduction.whnf ty_f, pat with
      | Pi (_,_,a,b), _ ->
          let (bo2,u) = check ctx a pat in
          let te = mk_App f u [] in
          let ty =
            if bo2 then ( try subst_check b u with SubstCheck -> error_joker te)
            else Subst.subst b u
          in ( (bo1||bo2,te) , ty )
      | _, _ -> error_product f ctx ty_f

  in snd (synth ctx pat)

(******************************************************************************)

let check_context (ctx:context) : unit =
  let aux ctx0 a = is_a_type ctx0 (snd a); a::ctx0
  in ignore (List.fold_left aux [] (List.rev ctx))

let check_rule r =
  let _ = check_context r.ctx in
  let ty = infer_pat r.ctx (Pattern(r.l,Env.get_name (),r.id,r.args)) in
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
