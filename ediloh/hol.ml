open Basic

let compile_proofs = ref true

(* TODO: suppress ctx in the last translation *)
type name = ident * ident

type _ty =
  | VarTy of ident
  | Arrow of _ty * _ty
  | OpType of name * _ty list
  | Bool

type ty =
  | ForallK of ident * ty
  | Type of _ty

type ty_subst = (Basic.ident * _ty) list

type _term =
  | Forall of ident * _ty * _term
  | Impl of _term * _term
  | VarTerm of ident * _ty
  | Const of name * _ty * ty_subst
  | App of _term * _term list
  | Lam of ident * _ty * _term

type term =
  | ForallT of ident * term
  | Term of _term

let mk_App f args =
  match f with
    | App (f',args') -> App (f',args'@args)
    | _ -> App(f,args)


type ctx = int list

type ty_ctx = Basic.ident list

type term_ctx = (Basic.ident * ty) list

type te_subst = (Basic.ident * _term) list

type proof_ctx = (Basic.ident * term) list

type 'a dir = Fold of 'a | Unfold of 'a

type rw = Delta of name *  _ty * ty_subst | Beta of _term


type _proof =
  | Lemma of name * term * ty_subst
  | Assume of _term * ty_subst
  | ForallI of ident * _ty * _prooft
  | ImplI of _term * _prooft
  | ForallE of _prooft * _term
  | ImplE of _prooft * _prooft
  | RewriteU of ctx * rw dir * _prooft



and _prooft =
  {
    _term:_term;
    _proof:_proof;
  }

type proof =
  | ForallP of ident * prooft
  | RewriteF of ctx * rw dir * prooft
  | Proof of _prooft

and prooft =
  {
    term:term;
    proof:proof;
  }

type obj =
  | Cst of name * ty * term option
  | TyOp of name * _ty list
  | Thm of name * term * prooft option

let name_eq (md,id) (md',id') = Basic.ident_eq md md' && Basic.ident_eq id id'

let alpha_eq ctx id id' =
  if Basic.ident_eq id id' then
    true
  else
    try
      id' = List.assoc id ctx
    with _ -> false




let rec _ty_eq ctx left right : bool =
  match (left,right) with
  | Bool, Bool -> true
  | VarTy x, VarTy y -> alpha_eq ctx x y
  | Arrow(tyll, tylr), Arrow(tyrl, tyrr) -> _ty_eq ctx tyll tyrl && _ty_eq ctx tylr tyrr
  | OpType(name, tys), OpType(name', tys') ->
    name_eq name name' && List.for_all2 (fun l r -> _ty_eq ctx l r) tys tys'
  | _, _ -> false

let rec ty_eq ctx left right : bool =
  match (left,right) with
  | Type(ty), Type(ty') -> _ty_eq ctx ty ty'
  | ForallK(id, ty), ForallK(id', ty') -> ty_eq ((id,id')::ctx) ty ty'
  | _, _ -> false

(* FIXME: type ident and var ident should be always different. This can be fixed by chaning the grammar of ident to make a distinction between type ident and var ident *)
let rec _term_eq ctx left right =
  match (left,right) with
  | VarTerm(id,_ty), VarTerm(id',_ty') -> alpha_eq ctx id id' && _ty_eq ctx _ty _ty'
  | Const(name,_ty,_), Const(name',_ty',_) -> name_eq name name' && _ty_eq ctx _ty _ty'
  | Lam(id,_ty, _te), Lam(id',_ty', _te') -> _ty_eq ctx _ty _ty' && _term_eq ((id,id')::ctx) _te _te'
  | Impl(_tel,_ter), Impl(_tel',_ter') -> _term_eq ctx _tel _tel' && _term_eq ctx _ter _ter'
  | Forall(id,_ty,_te), Forall(id',_ty',_te') -> _ty_eq ctx _ty _ty'
                                                 && _term_eq ((id,id')::ctx) _te _te'
  | App(_te, _tes), App(_te', _tes') ->
    _term_eq ctx _te _te' && List.for_all2 (fun l r -> _term_eq ctx l r) _tes _tes'
  | _,_ -> false

let rec term_eq ctx left right =
  match (left,right) with
  | ForallT(id,term), ForallT(id',term') -> term_eq ((id,id')::ctx) term term'
  | Term(_te), Term(_te') -> _term_eq ctx _te _te'
  | _, _ -> false

type decl = loc * ident * Term.term

type compile_type_err = TypeError of Term.term

type compile_term_err = TermError of Term.term | UntypedLambda of Term.term

type compile_decl_err =
  | DeclarationError of decl
  | DeclarationTypeError of compile_type_err * decl
  | DeclarationTermError of compile_term_err * decl

type compile_proof_err = ProofError of Term.term

type compile_defn_err =
  | DefinitionError of decl * Term.term
  | DefinitionTypeError of compile_type_err * decl * Term.term
  | DefinitionTermError of compile_term_err * decl * Term.term
  | DefinitionProofError of compile_proof_err * decl * Term.term

let print_name out (md,id) =
  Format.fprintf out "%a.%a" Pp.print_ident md Pp.print_ident id

let dloc = Basic.dloc
let hol = hstring "hol"

let name_dedukti (md,id) = Term.mk_Const dloc md id

let rec hol__type_dedukti ty =
  match ty with
  | VarTy id ->
    (* painful to get the correct index, so this is a hack cause of Pp.subst function *)
    Term.mk_DB dloc id 1000
  | Arrow(tyl,tyr) -> Term.mk_App (Term.mk_Const dloc hol (hstring "arrow"))
                        (hol__type_dedukti tyl) [(hol__type_dedukti tyl)]
  | OpType(name,tys) ->
    List.fold_left (fun term arg -> Term.mk_App term (hol__type_dedukti arg) [])
    (name_dedukti name) tys
  | Bool -> Term.mk_Const dloc hol (hstring "prop")

let rec hol_type_dedukti ty =
  match ty with
  | ForallK(var,ty) ->
    Term.mk_App (Term.mk_Const dloc hol (hstring "forall_kind_type"))
      (Term.mk_Lam dloc var (Some (Term.mk_Const dloc hol (hstring "type")))
         (hol_type_dedukti ty)) []
  | Type(ty) -> hol__type_dedukti ty

(* FIXME: printing is won't work to print dedukti code. Some parenthesis are missing but right now it is only used for debugging *)
let rec print_hol__type out ty = Pp.print_term out (hol__type_dedukti ty)

let rec print_hol_type out ty = Pp.print_term out (hol_type_dedukti ty)

let rec print_hol__prooft out pr =
  match pr._proof with
  | Lemma _ -> Format.fprintf out "lemma@.";
  | Assume _ -> Format.fprintf out "assume@.";
  | ForallI(_,_,pr) -> Format.fprintf out "forallI@."; print_hol__prooft out pr
  | ImplI(_,pr) -> Format.fprintf out "ImplI@."; print_hol__prooft out pr
  | ForallE(pr,_) -> Format.fprintf out "forallE@."; print_hol__prooft out pr
  | ImplE(prl,prr) -> Format.fprintf out "ImplE@.";print_hol__prooft out prl;print_hol__prooft out prr;
  | RewriteU(_,_,pr) -> Format.fprintf out "rewriteU@."; print_hol__prooft out pr

let rec print_hol_prooft out pr =
  match pr.proof with
  | ForallP(_,pt) -> Format.fprintf out "forallP@."; print_hol_prooft out pt
  | RewriteF(_,_,pt) -> Format.fprintf out "rewriteU@."; print_hol_prooft out pt
  | Proof(_pt) -> print_hol__prooft out _pt

let rec hol__term_dedukti te =
  match te with
  | Forall(id,_ty,_te) ->
    let ty' = (hol__type_dedukti _ty) in
    Term.mk_App (Term.mk_Const dloc hol (hstring "forall"))
       ty' [(Term.mk_Lam dloc id
              (Some (Term.mk_App (Term.mk_Const dloc hol (hstring "eta")) ty' []))
              (hol__term_dedukti _te))]
  | Impl(_tel,_ter) ->
    Term.mk_App (Term.mk_Const dloc hol (hstring "impl"))
      (hol__term_dedukti _tel) [(hol__term_dedukti _ter)]
  | VarTerm(ident,_) -> Term.mk_DB dloc ident 1000
  | Const(name,_,_) -> name_dedukti name
  | Lam(id, _ty, _term) -> Term.mk_Lam dloc id (Some (hol__type_dedukti _ty)) (hol__term_dedukti _term)
  | App(f,args) ->
    List.fold_left (fun t arg -> Term.mk_App t (hol__term_dedukti arg) []) (hol__term_dedukti f) args

let rec hol_term_dedukti te =
  match te with
  | ForallT(var,te) ->
    Term.mk_App (Term.mk_Const dloc hol (hstring "forall_kind_prop"))
      (Term.mk_Lam dloc var (Some (Term.mk_Const dloc hol (hstring "type")))
         (hol_term_dedukti te)) []
  | Term te -> hol__term_dedukti te

let print_ty_subst out subst =
  let print_item out (id,_ty) =
    Format.fprintf out "%a[%a]" Pp.print_ident id print_hol__type _ty
  in
  Format.fprintf out "%a" (Pp.print_list "@." print_item) subst

let rec print_hol__term out te = Pp.print_term out (hol__term_dedukti te)
let rec print_hol_term out te = Pp.print_term out (hol_term_dedukti te)

let const_of_name (md,id) = Term.mk_Const dloc md id

exception CompileTypeError of compile_type_err

exception CompileTermError of compile_term_err

exception CompileProofError of compile_proof_err



module type Trace =
sig

  val annotate : term -> prooft -> prooft

  val _annotate : _term -> _prooft -> _prooft
end


let hol_module = hstring "hol"
let hol_type = hstring "type"
let hol_eta = hstring "eta"
let hol_arrow = hstring "arrow"
let hol_forall = hstring "forall"
let hol_impl = hstring "impl"
let hol_prop = hstring "prop"
let hol_eps = hstring "eps"
let hol_forall_kind_type = hstring "forall_kind_type"
let hol_forall_kind_prop = hstring "forall_kind_prop"

let (===) = Basic.ident_eq

let is_hol_const c t =
  match t with
  | Term.Const(_, m, id) -> (m === hol_module) &&  (c === id)
  | _ -> false

let is_type t =
  match t with
  | Term.App(cst, ty, _) when is_hol_const hol_eta cst -> true
  | _ -> false

let is_term t =
  match t with
  | Term.App(cst, ty, _) when is_hol_const hol_eps cst -> true
  | _ -> false


let rec poly_subst_ty (subst:ty_subst) (ty:ty) : _ty =
  match ty with
  | ForallK(var, ty') ->
    assert(List.mem_assoc var subst);
    poly_subst_ty subst ty'
  | Type(ty') -> poly_subst__ty subst ty'

and poly_subst__ty (subst:ty_subst) (ty:_ty) : _ty =
  match ty with
  | VarTy(var) ->
    if List.mem_assoc var subst then
      List.assoc var subst
    else
      VarTy(var)
  | Arrow(tyl,tyr) ->
    let tyl' = poly_subst__ty subst tyl in
    let tyr' = poly_subst__ty subst tyr in
    Arrow(tyl',tyr')
  | OpType(name,tys) ->
    OpType(name, List.map (poly_subst__ty subst) tys)
  | Bool -> Bool

let merge sub subst =
  List.fold_left (fun sub (id,_ty) -> (id,poly_subst__ty subst _ty)::sub) [] sub


let rec poly_subst_te (subst:ty_subst) (te:term) : _term =
  match te with
  | ForallT(var, te') ->
    assert(List.mem_assoc var subst);
    poly_subst_te subst te'
  | Term(te') -> poly_subst__te subst te'

and poly_subst__te (subst:ty_subst) (te:_term) : _term =
  match te with
  | Forall(var,_ty, _term) ->
    let _term' = poly_subst__te subst _term in
    Forall(var, poly_subst__ty subst _ty, _term')
  | Impl(_tel, _ter) ->
    let _tel' = poly_subst__te subst _tel in
    let _ter' = poly_subst__te subst _ter in
    Impl(_tel', _ter')
  | VarTerm(id, _ty) ->
    let _ty' = poly_subst__ty subst _ty in
    VarTerm(id, _ty')
  | App(f,_tes) ->
    let _tes' = List.map (poly_subst__te subst) _tes in
    let f' = poly_subst__te subst f in
    mk_App f' _tes'
  | Const(name, _ty, sub) ->
    let subst' = merge sub subst in
    let _ty' = poly_subst__ty subst _ty in
    Const(name, _ty', subst')
  | Lam(id, _ty, _te) ->
    let _te' = poly_subst__te subst _te in
    let _ty' = poly_subst__ty subst _ty in
    Lam(id, _ty', _te')

let rec term_subst__te (subst:te_subst) (te:_term) : _term =
  match te with
  | Forall(var,_ty,_term) ->
    let _term' = term_subst__te subst _term in
    Forall(var, _ty, _term')
  | Impl(_tel, _ter) ->
    let _tel' = term_subst__te subst _tel in
    let _ter' = term_subst__te subst _ter in
    Impl(_tel', _ter')
  | VarTerm(id, _ty) ->
    if List.mem_assoc id subst then
      List.assoc id subst
    else
      te
  | App(f,_tes) ->
    let _tes' = List.map (term_subst__te subst) _tes in
    let f' = term_subst__te subst f in
    mk_App f' _tes'
  | Const _ -> te
  | Lam(id, _ty, _te) ->
    let _te' = term_subst__te subst _te in
    Lam(id, _ty, _te')

let rec poly_var_of_ty (ty:ty) : ident list =
  match ty with
  | Type _ -> []
  | ForallK(id, ty') ->
    let vars = poly_var_of_ty ty' in
    id::vars

let rec poly_var_of_te (te:term) : ident list =
  match te with
  | Term _ -> []
  | ForallT(id, te') ->
    let vars = poly_var_of_te te' in
    id::vars

let mk_name md id = md,id

let rec compile_tyOp ty_ctx md id args =
  let args' = List.map (compile__type ty_ctx) args in
  OpType(mk_name md id, args')

and compile_type (ty_ctx:ty_ctx) (ty:Term.term) : ty =
  match ty with
  | Term.App(c, Term.Lam(_, var, _, ty), []) when is_hol_const hol_forall_kind_type c ->
    let ty' = compile_type (var::ty_ctx) ty in
    ForallK(var, ty')
  | _ -> Type (compile__type ty_ctx ty)

and compile__type (ty_ctx:ty_ctx) (ty:Term.term) : _ty =
  match ty with
  | Term.Const(_,md,id) when is_hol_const hol_prop ty -> Bool
  | Term.App(c,left,[right]) when is_hol_const hol_arrow c ->
    let left' = compile__type ty_ctx left in
    let right' = compile__type ty_ctx right in
    Arrow(left',right')
  | Term.App(Term.Const(_,md,id),a,args) -> compile_tyOp ty_ctx md id (a::args)
  | Term.Const(_,md,id) -> compile_tyOp ty_ctx md id []
  | Term.DB(_,var,_) ->
    if List.mem var ty_ctx then
      VarTy(var)
    else
      assert false
  | _ ->
    raise (CompileTypeError(TypeError(ty)))

let compile_eta_type (ty_ctx:ty_ctx) (ty:Term.term) : ty =
  match ty with
  | Term.App(cst, a, []) when is_hol_const hol_eta cst -> compile_type ty_ctx a
  | _ -> assert false

let ty_of_const lc md id =
  match Env.get_type lc md id with
  | OK ty -> ty
  | Err er -> Errors.fail_signature_error er

let rec compile__term (ty_ctx:ty_ctx) (te_ctx:term_ctx) (te:Term.term) : _term =
  let lookup_ty var =
    if List.mem_assoc var te_ctx then
      List.assoc var te_ctx
    else
      assert false
  in
  match te with
  | Term.App(cst, ty, [Term.Lam(_,id, Some tyvar, te)]) when is_hol_const hol_forall cst ->
    let ty' = compile__type ty_ctx ty in
    let te' = compile__term ty_ctx ((id,Type ty')::te_ctx) te in
    Forall(id, ty', te')
  | Term.App(cst, tel, [ter]) when is_hol_const hol_impl cst ->
    let tel' = compile__term ty_ctx te_ctx tel in
    let ter' = compile__term ty_ctx te_ctx ter in
    Impl(tel',ter')
  | Term.Const(lc,md,id) ->
    let ty = ty_of_const lc md id in
    let ty' = compile_eta_type ty_ctx ty in
    begin
      match ty' with
      | ForallK(var,te) -> assert false
      | Type(ty) -> Const(mk_name md id, ty, [])
    end
  | Term.DB(_,var,_) ->
    let ty = lookup_ty var in
    begin
      match ty with
      | ForallK(var,te) -> assert false
      | Type(ty) -> VarTerm(var,ty)
    end
  | Term.Lam(_,id, Some cst, te) when is_hol_const hol_type cst ->
    compile__term (id::ty_ctx) te_ctx te
  | Term.Lam(_,id, Some tyvar, te) ->
    let ty = compile_eta_type ty_ctx tyvar in
    let _ty' =
      begin
        match ty with
        | ForallK(var,te) -> assert false
        | Type(ty) -> ty
      end
    in
    let te' = compile__term ty_ctx ((id,ty)::te_ctx) te in
    Lam(id,_ty', te')
  | Term.Lam(_, _, None, _) ->
    raise(CompileTermError(UntypedLambda(te)))
  | Term.App(Term.Const(lc,md,id),a,args) ->
    let ty = ty_of_const lc md id in
    let ty' = compile_eta_type ty_ctx ty in
    let ty'', subst, args' = _ty_of_ty ty_ctx te_ctx ty' (a::args) in
    App(Const(mk_name md id, ty'', subst), args')
  | Term.App(Term.DB(_,var,_),a,args) ->
    let ty = lookup_ty var in
    let ty', subst, args' = _ty_of_ty ty_ctx te_ctx ty (a::args) in
    App(VarTerm(var, ty'), args')
  | _ -> raise(CompileTermError(TermError(te)))


and _ty_of_ty (ty_ctx:ty_ctx) (te_ctx:term_ctx) (ty:ty) (args:Term.term list)
  : _ty * ty_subst * _term list =
  let rec split l n =
    if n = 0 then
      [],l
    else
      match l with
      | [] -> assert false
      | x::t ->
        let poly,args = split t (n-1) in
        x::poly,args
  in
  let poly_vars = poly_var_of_ty ty in
  let n = List.length poly_vars in
  let poly_args,args = split args n in
  let poly_args' = List.map (compile__type ty_ctx) poly_args in
  let subst = List.combine poly_vars poly_args' in
  let _ty = poly_subst_ty subst ty in
  let args' = List.map (compile__term ty_ctx te_ctx) args in
  _ty, subst, args'

and compile_term (ty_ctx:ty_ctx) (te_ctx:term_ctx) (te:Term.term) : term =
  match te with
  | Term.App(cst, Term.Lam(_,x, Some ty, te'), []) when is_hol_const hol_forall_kind_prop cst ->
    assert (is_hol_const hol_type ty);
    ForallT(x,compile_term (x::ty_ctx) (te_ctx) te')
  | _ -> Term (compile__term ty_ctx te_ctx te)

let compile_eps_term (ty_ctx:ty_ctx) (te_ctx:term_ctx) (te:Term.term) : term =
  match te with
  | Term.App(cst, a, []) when is_hol_const hol_eps cst -> compile_term ty_ctx te_ctx a
  | _ -> assert false

let is_delta_rw cst =
  match cst with
  | Term.Const(_,_,id) -> Str.(string_match (regexp "__eq_\\|__eq_sym") (string_of_ident id) 0)
  | _ -> false

let get_infos_of_delta_rw md id = Str.(
    let id = string_of_ident id in
    if string_match (regexp "\\(__eq__\\)\\(.*\\)") id 0 then
      let id = matched_group 2 id in
      let cst = Term.mk_Const Basic.dloc md (hstring id) in
      match Env.reduction Reduction.OneStep cst with
      | OK te -> (hstring id),te
      | Err err -> Errors.fail_env_error err
    else
      assert false
)

let rec subst var u t =
  match t with
  | VarTerm(id,_ty) when Basic.ident_eq id var ->
    u
  | Lam(id,_ty,_te) when not @@ Basic.ident_eq id var -> Lam(id,_ty,subst var u _te)
  | Forall(id,_ty,_te) -> Forall(id,_ty, subst var u _te)
  | Impl(_tel, _ter) -> Impl(subst var u _tel, subst var u _ter)
  | App(f,args) -> App(subst var u f, List.map (subst var u) args)
  | _ -> t

let beta_step left =
  match left with
  | App(Lam(id,_ty,te), [u]) ->
    subst id u te
  | App(Lam(id,_ty,te), u::args) ->
    App(subst id u te, args)
  | _ -> assert false

let rec snf_beta _term =
  match _term with
  | Forall(id,_ty,_te) -> Forall(id,_ty,snf_beta _te)
  | Impl(_tel,_ter) ->
    Impl(snf_beta _tel, snf_beta _ter)
  | Lam(id,_ty,_te) -> Lam(id,_ty,snf_beta _te)
  | App(Lam(id,_ty,_te), [u]) ->
    beta_step _term
  | App(Lam(id,_ty,_te), u::args) ->
    let f' = beta_step (App(Lam(id,_ty,_te),[u])) in
    List.fold_left (fun t arg -> mk_App t [snf_beta arg]) f' args
  | _ -> _term


module Trace : Trace = struct


  type trace = ctx * rw dir

  let bind i pl =
    match pl with
    | None -> None
    | Some(ctx, rw) -> Some(i::ctx, rw)

  let rec _compare ctx left right : (ctx * rw dir option) option =
    (* assume barendregt convention *)
    let same_lambda x _ty te u t' =
      match t' with
      | App(Lam(x',_ty',te'), _) -> Basic.ident_eq x x' && _ty_eq ctx _ty _ty'
      | _ -> false
    in
    match (left,right) with
    | VarTerm(id,_ty), VarTerm(id',_ty') when alpha_eq ctx id id' && _ty_eq ctx _ty _ty' -> None
    | Const(name, _ty,_), Const(name', _ty',_) when name_eq name name' && _ty_eq ctx _ty _ty' -> None
    | Lam(id, _ty, _te), Lam(id', _ty', _te') when _ty_eq ctx _ty _ty' ->
      bind 0 (_compare ((id,id')::ctx) _te _te')
    | Forall(id, _ty, _te), Forall(id', _ty', _te') when _ty_eq ctx _ty _ty' ->
      bind 0 (_compare ((id,id')::ctx) _te _te')
    | Impl(_tel, _ter), Impl(_tel', _ter') ->
      begin
        match _compare ctx _tel _tel' with
        | None -> bind 1 (_compare ctx _ter _ter')
        | Some tr -> bind 0 (Some tr)
      end
    | App(Lam(x,_ty,te) as f,u::args), _ when not (same_lambda x _ty te u right) ->
      Some ([], Some(Unfold(Beta(App(f,[u])))))
    | _,App(Lam(x,_ty,te) as f,u::args) when not (same_lambda x _ty te u left) ->
      Some ([], Some(Fold(Beta(App(f,[u])))))
    | Const(name, _ty, subst),_ -> Some ([], Some(Fold(Delta(name,_ty,subst))))
    | _,Const(name, _ty, subst) -> Some ([], Some(Unfold(Delta(name,_ty,subst))))
    | App(f,args), App(f', args') ->
      snd @@
      List.fold_left2 (fun (i,tr) l r ->
          match tr with
          | None ->
            begin
              match _compare ctx l r with
              | None -> (i+1,None)
              | Some tr' -> (i+1, bind i (Some tr'))
            end
          | Some tr' -> (i+1,tr)) (0,None) (f::args) (f'::args')

    | _,_ -> Some ([0], None)

  let rec compare ctx left right : (ctx * rw dir option) option =
    match (left,right) with
    | Term(_te), Term(_te') -> bind 0 (_compare ctx _te _te')
    | ForallT(id,te), ForallT(id', te') ->
      bind 0 (compare ((id,id')::ctx) te te')
    | _, _ -> Some ([], None)

  let print_ctx fmt ctx =
    Pp.print_list " " (fun fmt d -> Format.fprintf fmt "%d" d) fmt ctx

    (*
    match _pt._term, ctx with
    | VarTerm _, _ -> assert false
    | Const(name,_ty, subst), [] ->
      let cst = const_of_name name in
      let te = Env.unsafe_one_step cst in
      let te'= compile_term [] [] te in
      let _te = poly_subst_te subst te' in
      let proof = Rewrite(name, [], _pt) in
      {_term=_te;_proof=proof}
    | _ -> assert false
       *)
  let rec _replace t ctx u =
    match t, ctx with
    | _, [] -> u
    | Forall(id,_ty,_term), 0::ctx' -> Forall(id,_ty,_replace _term ctx' u)
    | Impl(tel,ter), i::ctx' ->
      let tel' = if i = 0 then _replace tel ctx' u else tel in
      let ter' = if i = 1 then _replace ter ctx' u else ter in
      Impl(tel',ter')
    | Const _, [0] -> u
    | VarTerm _, [0] -> u
    | Lam(id,ty, _te), 0::ctx' -> Lam(id,ty,_replace _te ctx' u)
    | App(f,args'), i::ctx' ->
      let f' =
      if i = 0 then
        App(_replace f ctx' u, args')
      else
        f
      in
      let (i,t) = List.fold_left (fun (i,t) arg ->
          let arg' = if i = 0 then _replace arg ctx' u else arg in
          (i-1),mk_App t [arg']) ((i-1),f') args' in
      assert (i<0);
      t
    | _ -> assert false

  let rec replace t ctx u =
    match t,ctx with
    | ForallT(x,te),0::ctx' -> ForallT(x,replace te ctx' u)
    | Term(_te), 0::ctx' -> Term(_replace _te ctx' u)
    | _ -> assert false

  let apply__trace (ctx,rw) _pt =
      match rw with
    | Fold(Delta(name,_ty,subst)) ->
      let _proof = RewriteU(ctx,rw,_pt) in
      let _term = _replace _pt._term ctx (Const(name,_ty,subst)) in
      {_term;_proof}
    | Fold(Beta(term)) ->
      let _proof = RewriteU(ctx,rw,_pt) in
      let _term = _replace _pt._term ctx (beta_step term) in
      {_term;_proof}
    | Unfold(Beta(term)) ->
      let _proof = RewriteU(ctx,rw,_pt) in
      let _term = _replace _pt._term ctx term in
      {_term;_proof}
    | _ -> failwith "it would be nice if this exception was not raised"

  let apply_trace (ctx,rw) pt =
    match rw with
    | Fold(Delta(name,_ty,subst)) ->
      let proof = RewriteF(ctx,rw,pt) in
      let term = replace pt.term ctx (Const(name,_ty,subst)) in
      {term;proof}
    | Fold(Beta(term)) ->
      let proof = RewriteF(ctx,rw,pt) in
      let term = replace pt.term ctx (beta_step term) in
      {term;proof}
    | Unfold(Beta(term)) ->
      let proof = RewriteF(ctx,rw,pt) in
      let term = replace pt.term ctx term in
      {term;proof}
    | _ -> failwith "it would be nice if this exception was not raised"


  let rec annotate (t:term) (pt:prooft) : prooft =
    (* Format.eprintf "annotate:@. %a@.%a@." print_hol_term t print_hol_term pt.term; *)
    let to_trace ctx rw =
      match rw with
      | Some(rw) -> (ctx, rw)
      | None ->
        Errors.fail Basic.dloc "Contextual error: The terms %a and %a seems not to be convertible. They differ at position %a"
          print_hol_term t print_hol_term pt.term print_ctx ctx
    in
    match compare [] t pt.term with
    | None -> pt
    | Some (ctx,rw) ->
      let tr = to_trace ctx rw in
      annotate t (apply_trace tr pt)




  let rec _annotate (_t:_term) (_pt:_prooft) : _prooft =
    (* Format.eprintf "_annotate:@. %a@.%a@." print_hol__term _t print_hol__term _pt._term; *)
    let to_trace ctx rw =
      match rw with
      | Some(rw) -> (ctx, rw)
      | None ->
        Errors.fail Basic.dloc "Contextual error: The terms %a and %a seems not to be convertible. They differ at position %a"
          print_hol__term _t print_hol__term _pt._term print_ctx ctx
    in
    match _compare [] _t _pt._term with
    | None -> _pt
    | Some (ctx,rw) ->
      let ctx,rw = to_trace ctx rw in
      match rw with
      | Fold(Delta(name,_ty,subst)) ->
        let cst = const_of_name name in
        let te = Env.unsafe_one_step cst in
        let te'= compile_term [] [] te in
        let _te = poly_subst_te subst te' in
        let _t' = _replace _t ctx _te in
        let _pt' = _annotate _t' _pt in
        let _proof = RewriteU(ctx,rw,_pt') in
        let _term = _replace _pt'._term ctx (Const(name,_ty,subst)) in
        {_term;_proof}
      | Fold(Beta(term)) ->
        let _proof = RewriteU(ctx,rw,_pt) in
        let _term = _replace _pt._term ctx (beta_step term) in
        _annotate _t ({_term;_proof})
      | Unfold(Beta(term)) ->
        let _t' = _replace _t ctx (beta_step term) in
        let _pt' = _annotate _t' _pt in
        let _proof = RewriteU(ctx,rw,_pt') in
        let _term = _replace _pt'._term ctx term in
        {_term;_proof}
      | _ -> failwith "it would be nice if this exception was not raised"
end


let rec compile__proof (ty_ctx:ty_ctx) (te_ctx:term_ctx) (pf_ctx:proof_ctx) proof : _prooft =
  match proof with
  | Term.Lam(_,x, Some ty, proof) when is_type ty ->
    let ty' = compile_eta_type ty_ctx ty in
    let _prooft' = compile__proof ty_ctx ((x,ty')::te_ctx) pf_ctx proof in
    let _ty' =
      match ty' with
      | ForallK _ -> assert false
      | Type(_ty) -> _ty
    in
    let _term = Forall(x,_ty', _prooft'._term) in
    let _proof = ForallI(x,_ty', _prooft') in
    {_term;_proof}
  | Term.Lam(_,x, Some te, proof) when is_term te ->
    let te' = compile_eps_term ty_ctx te_ctx te in
    let _prooft' = compile__proof ty_ctx te_ctx ((x,te')::pf_ctx) proof in
    let _te' =
      match te' with
      | ForallT _ -> assert false
      | Term(_te) -> _te
    in
    let _term = Impl(_te', _prooft'._term) in
    let _proof = ImplI(_te', _prooft') in
    {_term;_proof}
  | Term.DB(_,id,_) ->
    if List.mem_assoc id pf_ctx then
      let te' = List.assoc id pf_ctx in
      let _term =
        match te' with
        | ForallT _ -> assert false
        | Term(_te) -> _te
      in
      let _proof = Assume(_term,[]) in
      {_term;_proof}
    else
      assert false
  | Term.Const(lc,md,id) ->
    let te =
      match Env.get_type lc md id with
      | OK ty -> ty
      | Err err -> Errors.fail_signature_error err
    in
    let te' = compile_eps_term ty_ctx te_ctx te in
    let _term =
      match te' with
      | ForallT _ -> assert false
      | Term(_te) -> _te
    in
    let _proof = Lemma((md,id),te', []) in
    {_term;_proof}
    (*
  | Term.App(Term.Const(_,md,id) as rw, ctx, cst::args) when is_delta_rw rw ->
    let id,te = get_infos_of_delta_rw md id in
    let term =
      match Env.reduction ~red:Tracer.only_beta Reduction.OneStep (Term.mk_App ctx te []) with
      | OK te -> te
      | Err err -> Errors.fail_env_error err
    in
    let term' = compile_term ty_ctx te_ctx term in
    let _term', subst, args = _te_of_te ty_ctx te_ctx term' args in
    let prooft' = {_term=_term'; _proof=Lemma((md,id),term', subst)} in
    begin
      match ctx with
      | Term.Lam(_, var, Some ty, te) ->
        let ty' = compile_type ty_ctx ty in
        let _proof = DeltaU((md,id),(id,compile_term ty_ctx ((var,ty')::te_ctx) te), prooft') in
        let prooft = {prooft' with _proof} in
        compile_app ty_ctx te_ctx pf_ctx prooft args
      | _ -> assert false
    end *)
  | Term.App(Term.Const(lc,md,id),a,args) ->
    let te =
      match Env.get_type lc md id with
      | OK ty -> ty
      | Err err -> Errors.fail_signature_error err
    in
    let te' = compile_eps_term ty_ctx te_ctx te in
    let _te', subst, args = _te_of_te ty_ctx te_ctx te' (a::args) in
    let prooft  = {_term=_te'; _proof= Lemma((md,id),te', subst)} in
    compile_app ty_ctx te_ctx pf_ctx prooft args
  | Term.App(Term.DB(_,var,_),a,args) ->
    let te' =
      if List.mem_assoc var pf_ctx then
        List.assoc var pf_ctx
      else
        assert false
    in
    let _te', subst, args = _te_of_te ty_ctx te_ctx te' (a::args) in
    let prooft:_prooft = {_term=_te'; _proof= Assume(_te', subst)} in
    compile_app ty_ctx te_ctx pf_ctx prooft args
  | _ -> failwith "todo proof"

and _te_of_te (ty_ctx:ty_ctx) (te_ctx:term_ctx) (te:term) (args:Term.term list) =
  let rec split l n =
    if n = 0 then
      [],l
    else
      match l with
      | [] -> assert false
      | x::t ->
        let poly,args = split t (n-1) in
        x::poly,args
  in
  let poly_vars = poly_var_of_te te in
  let n = List.length poly_vars in
  let poly_args,args = split args n in
  let poly_args' = List.map (compile__type ty_ctx) poly_args in
  let subst = List.combine poly_vars poly_args' in
  let _te = poly_subst_te subst te in
  _te, subst, args

and compile_app (ty_ctx:ty_ctx) (te_ctx:term_ctx) (pf_ctx:proof_ctx) (prooft:_prooft)
    (args:Term.term list) : _prooft =
  let rec compile_arg (prooft:_prooft) arg = (*
    Format.printf "debug:  %a@." print_hol__term prooft._term;
    Format.printf "term: %a@." Pp.print_term arg; *)
    match prooft._term with
    | Forall(id,_ty,_term) ->
      let _term' = compile__term ty_ctx te_ctx arg in
      let term = term_subst__te [id,_term'] _term in
      let pt = {_term=term;_proof=ForallE(prooft, _term')} in
      let term' = snf_beta term in
      Trace._annotate term' pt
    | Impl(_tel, _telr) ->
      let prooft' = compile__proof ty_ctx te_ctx pf_ctx arg in
      {_term=_telr;_proof=ImplE(prooft, prooft')}
    | App(Const(name, _ty, subst), _tes) -> assert false
    | App(VarTerm(id, _ty), _tes) -> assert false
    | App _ -> failwith "don't know what to do"
    | Const _
    | VarTerm _ -> assert false
    | Lam _ -> assert false
  in
  List.fold_left compile_arg prooft args



let rec compile_proof (ty_ctx:ty_ctx) (te_ctx:term_ctx) (proof:Term.term) : prooft =
  match proof with
  | Term.Lam(_,x, Some ty, proof') when is_hol_const hol_type ty ->
    let prooft' = compile_proof (x::ty_ctx) te_ctx proof' in
    {term=ForallT(x,prooft'.term); proof=ForallP(x, prooft')}
    (*
  | Term.App(Term.Const(_,md,id) as cst, ctx, [proof]) when is_delta_rw cst ->
    let id,_ = get_infos_of_delta_rw md id in
    let arg = Term.mk_Const Basic.dloc md id in
    let term =
      match Env.reduction ~red:Tracer.only_beta Reduction.OneStep (Term.mk_App ctx arg []) with
      | OK te -> te
      | Err err -> Errors.fail_env_error err
    in
    let term' = compile_term ty_ctx te_ctx term in
    let prooft' = compile_proof ty_ctx te_ctx proof in
    begin
      match ctx with
      | Term.Lam(_, var, Some ty, te) ->
        let ty' = compile_type ty_ctx ty in
        {term=term';proof=DeltaF((md,id),(id,compile_term ty_ctx ((var,ty')::te_ctx) te), prooft')}
      | _ -> assert false
    end
*)
  | _ ->
    let _prooft' = compile__proof ty_ctx te_ctx [] proof in
    {term=Term(_prooft'._term); proof=Proof(_prooft')}

let compile_declaration (lc:loc) (id:ident) (te:Term.term) : (obj, compile_decl_err) error =
  let md = Env.get_name () in
  try
    match te with
    | Term.App(cst,a,[]) when is_hol_const hol_eta cst ->
      OK(Cst(mk_name md id, compile_type [] a, None))
    | Term.App(cst,a,[]) when is_hol_const hol_eps cst ->
      OK(Thm(mk_name md id, compile_term [] [] a, None))
    | Term.Const(_,md,id) when is_hol_const hol_type te ->
      OK(TyOp(mk_name md id, []))
    | _ -> Err(DeclarationError(lc,id,te))
  with
  | CompileTermError(err) ->
    Err(DeclarationTermError(err,(lc,id,te)))
  | CompileTypeError(err) ->
    Err(DeclarationTypeError(err,(lc,id,te)))

let fail_compile_declaration (err:compile_decl_err) : 'a =
  match err with
  | DeclarationError(lc,id,te) ->
    Errors.fail lc "Error while compiling the declaration '%a:%a'. It seems that the type is not recognized by the compiler." Pp.print_ident id Pp.print_term te
  | DeclarationTermError(err,(lc,id,te)) ->
    begin
      match err with
      | UntypedLambda(te) ->
        Errors.fail lc "Error while compiling the declaration '%a' as an axiom. The term %a has untyped lambdas." Pp.print_ident id Pp.print_term te
      | TermError(te) ->
        Errors.fail lc "Error while compiling the declaration '%a' as an axiom. The term %a seems not to be an hol theorem." Pp.print_ident id Pp.print_term te
    end
  | DeclarationTypeError(err,(lc,id,te)) ->
    begin
      match err with
      | TypeError(ty) ->
        Errors.fail lc "Error while compiling the declaration '%a' as a constant. The type %a seems not to be an hol type." Pp.print_ident id Pp.print_term te
    end

let rec has_beta' te =
  match te with
  | Forall(_,_,te) -> has_beta' te
  | Impl(tel,ter) -> has_beta' tel || has_beta' ter
  | VarTerm _
  | Const _ -> false
  | Lam(_,_,te) -> has_beta' te
  | App(Lam(_,_,_),[]) -> assert false
  | App(Lam(_,_,_),x::_) -> true
  | App(f,args) ->
    List.fold_left (fun b arg -> b || (has_beta' arg)) false (f::args)

let rec has_beta te =
  match te with
  | ForallT(_,te) -> has_beta te
  | Term te -> has_beta' te
(*
let prefix n =
    "_"^(string_of_int n)^"_"

let pre n id = hstring @@ prefix n ^(string_of_ident id)

let alpha_rename__proof n _proof = failwith "todo"

let alpha_rename_proof n proof =
  match proof with
  | ForallP(id, prooft) ->
    ForallP(id, alpha_rename n prooft)
  |

let rec alpha_rename__term n _term =
  match _term with
  | Forall(id,_ty,_te) ->
    Forall(pre n id, _ty, alpha_rename__term n _te)
  | Impl(_tel, _ter) ->
    Impl(alpha_rename__term n _tel, alpha_rename__term n _ter)
  | VarTerm(id,_ty) -> VarTerm(pre n id, _ty)
  | App(t,ts) ->
    App(alpha_rename__term n t, List.map (alpha_rename__term n) ts)
  | Lam(id,_ty, _te) ->
    Lam(pre n id, _ty, alpha_rename__term n _te)
  | _ -> _term

let rec alpha_rename_term n term =
  match term with
  | ForallT(id, te) -> ForallT(id, alpha_rename_term n te)
  | Term(_te) -> Term(alpha_rename__term n _te)


let alpha_rename n prooft =
  {
    proof = alpha_rename_proof n prooft.proof;
    term = alpha_rename_term n prooft.term
  }
  *)


let c = ref 0

let compile_definition (lc:loc) (id:ident) (ty:Term.term) (te:Term.term)
  : (obj, compile_defn_err) error =
  let md = Env.get_name () in
  try
    match ty with
    | Term.App(cst,a,[]) when is_hol_const hol_eta cst ->
      let te' = compile_term [] [] te in
      OK(Cst(mk_name md id, compile_type [] a, Some te'))
    | Term.App(cst,a,[]) when is_hol_const hol_eps cst ->
      let thm = compile_term [] [] a in
      let proof = compile_proof [] [] te in
      (*    let proof = alpha_rename !c proof in *)
      let proof' = Some (Trace.annotate thm proof) in
      incr c;
      OK(Thm(mk_name md id, thm, proof'))
    | _ -> Err(DefinitionError((lc,id,te),ty))
  with
  | CompileTermError(err) ->
    Err(DefinitionTermError(err,(lc,id,te),ty))
  | CompileTypeError(err) ->
    Err(DefinitionTypeError(err,(lc,id,te),ty))
  | CompileProofError(err) ->
    Err(DefinitionProofError(err,(lc,id,te),ty))
(*
           match compile_declaration lc id ty with
  | OK(obj) -> OK(obj)
  | Err err -> fail_compile_declaration err *)

let fail_compile_definition (err:compile_defn_err) : 'a =
  match err with
  | DefinitionError((lc,id,te),ty) ->
    Errors.fail lc "Error while compiling the definition '%a:%a:=%a'. It seems that the definition is not recognized by the compiler." Pp.print_ident id Pp.print_term te Pp.print_term ty
  | DefinitionTermError(err,(lc,id,te),ty) ->
    begin
      match err with
      | UntypedLambda(te) ->
        Errors.fail lc "Error while compiling the definition '%a'. The term %a has untyped lambdas." Pp.print_ident id Pp.print_term te
      | TermError(te) ->
        Errors.fail lc "Error while compiling the definition '%a'. The term %a seems not to be an hol theorem." Pp.print_ident id Pp.print_term te
    end
  | DefinitionTypeError(err,(lc,id,te),ty) ->
    begin
      match err with
      | TypeError(ty) ->
        Errors.fail lc "Error while compiling the definition '%a' as a constant. The type %a seems not to be an hol term." Pp.print_ident id Pp.print_term te
    end
  | DefinitionProofError(err,(lc,id,te),ty) ->
    begin
      match err with
      | ProofError(ty) ->
        Errors.fail lc "Error while compiling the definition '%a' as a proof. The term %a seems not to be an hol proof." Pp.print_ident id Pp.print_term te
    end
module OT = Openstt.OpenTheory

(* FIXME: rename this *)
let name_of_var var = OT.mk_name [] (string_of_ident var)

let compile_hol_name (md,id) =
  let md' = string_of_ident md in
  let id' = string_of_ident id in
  OT.mk_name [md'] id'


(* FIXME: ctx are unecessary. They can be useful to make some assertions *)
let rec compile_hol__type (ty_ctx:ty_ctx) (_ty:_ty) =
  match _ty with
  | VarTy(var) -> OT.mk_varType (name_of_var var)
  | Arrow(_tyl,_tyr) ->
    let _tyl' = compile_hol__type ty_ctx _tyl in
    let _tyr' = compile_hol__type ty_ctx _tyr in
    OT.mk_arrow_type _tyl' _tyr'
  | OpType(name, tys) ->
    let tyop' = OT.mk_tyOp (compile_hol_name name) in
    let tys' = List.map (compile_hol__type ty_ctx) tys in
    OT.ty_of_tyOp tyop' tys'
  | Bool -> OT.mk_bool_type

let rec compile_hol_type (ty_ctx:ty_ctx) (ty:ty) =
  match ty with
  | ForallK(var,te) -> compile_hol_type (var::ty_ctx) te
  | Type(te) -> compile_hol__type ty_ctx te


(* FIXME: ctx are unecessary. They can be useful to make some assertions *)
let rec compile_hol__term (ty_ctx:ty_ctx) (te_ctx:term_ctx) term =
  match term with
  | Forall(var,_ty, _te) ->
    let _ty' = compile_hol__type ty_ctx _ty in
    let lambda = Lam(var, _ty,_te) in
    let lambda' = compile_hol__term ty_ctx te_ctx lambda in
    OT.mk_forall_term lambda' _ty'
  | Impl(_tel, _ter) ->
    let _tel' = compile_hol__term ty_ctx te_ctx _tel in
    let _ter' = compile_hol__term ty_ctx te_ctx _ter in
    OT.mk_impl_term _tel' _ter'
  | App(f, args) ->
    let f' = compile_hol__term ty_ctx te_ctx f in
    let args' = List.map (compile_hol__term ty_ctx te_ctx) args in
    List.fold_left (fun app arg ->
        OT.mk_app_term app arg) f' args'
  | VarTerm(var,_ty) ->
    let _ty' = compile_hol__type ty_ctx _ty in
    OT.mk_var_term (OT.mk_var (name_of_var var) _ty')
  | Const(name, _ty, subst) ->
    let _ty' = compile_hol__type ty_ctx _ty in
    let cst = OT.const_of_name (compile_hol_name name) in
    OT.term_of_const cst _ty'
  | Lam(var,ty,_term) ->
    let _term' = compile_hol__term ty_ctx ((var,Type ty)::te_ctx) _term in
    let _ty' = compile_hol__type ty_ctx ty in
    let var' = OT.mk_var (name_of_var var) _ty' in
    OT.mk_abs_term var' _term'

let rec compile_hol_term (ty_ctx:ty_ctx) (te_ctx:term_ctx) term =
  match term with
  | ForallT(var,te) -> compile_hol_term (var::ty_ctx) te_ctx te
  | Term(te) -> compile_hol__term ty_ctx te_ctx te

let compile_ctx eq_proof (var,ctx) =
  let rec compile_ctx ctx =
    match ctx with
    | VarTerm(var', _ty) when var' = var -> eq_proof
    | _ -> failwith "todo compile_ctx"
  in
  compile_ctx ctx

let compile_hol_subst ty_ctx subst =
  let compile_binding (var,ty) = name_of_var var, compile_hol__type ty_ctx ty in
  List.map compile_binding subst


type ctx_proof =
  {
    prf:OT.thm OT.obj;
    left:OT.term OT.obj;
    right:OT.term OT.obj;
  }

let hol_const_of_name name _ty ty_subst =
  let cst = const_of_name name in
  let te = Env.unsafe_one_step cst in
  let te' = compile_term [] [] te in
  poly_subst_te ty_subst te'

let base_proof rw_proof =
  match rw_proof with
  | Unfold(Beta(left)) ->
    let left' = compile_hol__term [] [] left in
    let right' = compile_hol__term [] [] (beta_step left) in
    let p = OT.mk_betaConv left' in
    {prf=OT.mk_sym p; right=left';left=right'}
  | Fold(Beta(left)) ->
    let left' = compile_hol__term [] [] left in
    let right' = compile_hol__term [] [] (beta_step left) in
    {prf=OT.mk_betaConv left'; right=right'; left=left'}
  | Unfold(Delta(name,_ty,ty_subst)) ->
    let left = compile_hol__term [] [] @@ hol_const_of_name name _ty ty_subst in
    let right = compile_hol__term [] [] @@ Const(name,_ty,ty_subst) in
    let pr = OT.thm_of_const_name (compile_hol_name name) in
    {prf=pr;left;right}
  | Fold(Delta(name,_ty,ty_subst)) ->
    let left = compile_hol__term [] [] @@ hol_const_of_name name _ty ty_subst in
    let right = compile_hol__term [] [] @@ Const(name,_ty,ty_subst) in
    let pr = OT.thm_of_const_name (compile_hol_name name) in
    {prf=OT.mk_sym pr;left=right;right=left}

(* FIXME: ctx are not update *)
let rec compile_hol__proof (ty_ctx:ty_ctx) (te_ctx:term_ctx) (pf_ctx:proof_ctx) proof  =
  let open OT in
  match proof with
  | Lemma(name,term, subst) ->
    OT.comment "#Lemma %a.%a@." Pp.print_ident (fst name) Pp.print_ident (snd name);
    let proof =
      try
        thm_of_lemma (compile_hol_name name)
      with Failure _ ->
        mk_axiom (mk_hyp []) (compile_hol_term ty_ctx te_ctx term)
    in
    mk_subst proof (compile_hol_subst ty_ctx subst) []
  | Assume(_te, subst) ->
    OT.comment "@[#Assume %a@]@." print_hol__term _te;
    mk_subst (OT.mk_assume (compile_hol__term ty_ctx te_ctx _te)) (compile_hol_subst ty_ctx subst) []
  | ForallI(id,_ty, _prooft) ->
    OT.comment "#Forall (intro) %a@." Pp.print_ident id;
    let name = name_of_var id in
    OT.comment "#Forall (intro) type@.";
    let _ty = compile_hol__type ty_ctx _ty in
    OT.comment "#Forall (intro) term@.";
    let _term = compile_hol__term ty_ctx te_ctx _prooft._term in
    OT.comment "#Forall (intro) proof@.";
    let _proof = compile_hol__proof ty_ctx te_ctx pf_ctx _prooft._proof in
    OT.comment "#Forall (intro) rule@.";
    mk_rule_intro_forall name _ty _term _proof
  | ImplI(_term, _prooft) ->
    OT.comment "#Impl (intro)@.";
    let _proof = compile_hol__proof ty_ctx te_ctx pf_ctx _prooft._proof in
    OT.comment "#Impl (intro) left@.";
    let p = compile_hol__term ty_ctx te_ctx _term in
    OT.comment "#Impl (intro) right@.";
    let q = compile_hol__term ty_ctx te_ctx _prooft._term in
    OT.comment "#Impl (intro) rule@.";
    mk_rule_intro_impl _proof p q
  | ForallE(_prooft,_term) ->
    OT.comment "#Forall elim@.";
    let id,_ty,lam =
      match _prooft._term with
      | Forall(id,_ty,_term) -> id,_ty, Lam(id, _ty, _term)
      | _ -> assert false
    in
    OT.comment "#Forall (elim) type@.";
    let _ty' = compile_hol__type ty_ctx _ty in
    OT.comment "#Forall (elim) lambda@.";
    let lam' = compile_hol__term ty_ctx te_ctx lam in
    OT.comment "#Forall (elim) proof@.";
    let _proof' = compile_hol__proof ty_ctx ((id,Type _ty)::te_ctx) pf_ctx _prooft._proof in
    OT.comment "#Forall (elim) term@.";
    let _term' = compile_hol__term ty_ctx te_ctx _term in
    OT.comment "#Forall (elim) rule@.";
    mk_rule_elim_forall _proof' lam' _ty' _term'
  | ImplE(_prooftl,_prooftr) ->
    OT.comment "#Impl (elim)@.";
    let p,q =
      match _prooftl._term with
      | Impl(p,q) -> p,q
      | _ -> assert false
    in
    OT.comment "#Impl (elim) term p@.";
    let p' = compile_hol__term ty_ctx te_ctx p in
    OT.comment "#Impl (elim) term q@.";
    let q' = compile_hol__term ty_ctx te_ctx q in
    OT.comment "#Impl (elim) proof p=>q@.";
    let proofimpl = compile_hol__proof ty_ctx te_ctx pf_ctx _prooftl._proof in
    OT.comment "#Impl (elim) proof p@.";
    let proofp = compile_hol__proof ty_ctx te_ctx pf_ctx _prooftr._proof in
    OT.comment "#Impl (elim) rule@.";
    mk_rule_elim_impl proofp proofimpl p' q'
  | RewriteU(ctx,rw,_pt) ->
    OT.comment "#rewriteU eq proof@.";
    let eq_proof = (compile__ctx_proof (base_proof rw) ctx _pt._term).prf in
    OT.comment "#rewriteU proof@.";
    let proof' = compile_hol__proof ty_ctx te_ctx pf_ctx _pt._proof in
    OT.mk_eqMp proof' eq_proof

and compile__ctx_proof base_proof ctx _te =
  (* Format.eprintf "debug: %d, %a@." (List.length ctx) print_hol__term _te; *)
  match _te, ctx with
  | _, [] -> base_proof
  | Forall(id,_ty,_te), 0::ctx' ->
    OT.comment "#ctx forall@.";
    let pr = compile__ctx_proof base_proof ctx' _te in
    let ty' = compile_hol__type [] _ty in
    let prf = OT.mk_forall_equal pr.prf (name_of_var id) pr.left pr.right ty' in
    let lambda' t = OT.mk_abs_term (OT.mk_var (name_of_var id) ty') t in
    let left = OT.mk_forall_term (lambda' pr.left) ty' in
    let right = OT.mk_forall_term (lambda' pr.right) ty' in
    {prf;left;right}
  | Impl(tel,ter), i::ctx' ->
    OT.comment "#ctx impl left@.";
    let prl,prr =
      if i = 0 then
        let prl = compile__ctx_proof base_proof ctx' tel in
        let ter' = compile_hol__term [] [] ter in
        OT.comment "#ctx impl right 0@.";
        let prr = {prf=OT.mk_refl ter'; left=ter'; right=ter'} in
        prl,prr
      else if i = 1 then
        let prr = compile__ctx_proof base_proof ctx' ter in
        let tel' = compile_hol__term [] [] tel in
        OT.comment "#ctx impl right 1@.";
        let prl = {prf=OT.mk_refl tel'; left=tel'; right=tel'} in
        prl,prr
      else assert false
    in
    let prf = OT.mk_impl_equal prl.prf prr.prf prl.left prr.left prl.right prr.right in
    let left = OT.mk_impl_term prl.left prr.left in
    let right = OT.mk_impl_term prl.right prr.right in
    let pr = {prf;left;right} in
    pr
  | App(f,args), i::ctx' ->
    OT.comment "#ctx app@.";
    if i = 0 then
      let pr = compile__ctx_proof base_proof ctx' f in
      List.fold_left compile_app pr (List.map (compile_hol__term [] []) args)
    else(
      let f' = compile_hol__term [] [] f in
      let pr = {prf = OT.mk_refl f'; left=f'; right=f'} in
      let rec split l n =
        if n = 0 then
          match l with
          | [] -> assert false
          | x::t -> [],x,t
        else
          match l with
          | [] -> assert false
          | x::t -> let left,e,right = split t (n-1) in x::left,e,right
      in
      let left,arg,right = split args (i-1) in
      let prarg = compile__ctx_proof base_proof ctx' arg in
      let pr' = List.fold_left compile_app pr (List.map (compile_hol__term [] []) left) in
      let pr'' = {prf=OT.mk_appThm pr'.prf prarg.prf;
                  left = OT.mk_app_term pr'.left prarg.left;
                  right = OT.mk_app_term pr'.right prarg.right} in
      List.fold_left compile_app pr'' (List.map (compile_hol__term [] []) right))
  | Lam(id,_ty,_te), 0::ctx' ->
    OT.comment "#ctx lam@.";
    let pr = compile__ctx_proof base_proof ctx' _te in
    let ty' = compile_hol__type [] _ty in
    let lambda' t = OT.mk_abs_term (OT.mk_var (name_of_var id) ty') t in
    let left = lambda' pr.left in
    let right = lambda' pr.right in
    let prf = OT.mk_absThm (OT.mk_var (name_of_var id) ty') pr.prf in
    {prf;left;right}
  | _ -> assert false

and compile_app pr arg =
  { prf = OT.mk_appThm pr.prf (OT.mk_refl arg);
    left = OT.mk_app_term pr.left arg;
    right = OT.mk_app_term pr.right arg
  }

let rec compile_ctx_proof base_proof ctx te =
  match te, ctx with
  | _, [] -> base_proof
  | ForallT(id,term), 0::ctx' -> compile_ctx_proof base_proof ctx' term
  | Term(_term), 0::ctx' -> compile__ctx_proof base_proof ctx' _term
  | _ -> assert false

let rec compile_hol_proof (ty_ctx:ty_ctx) (te_ctx:term_ctx) (pf_ctx:proof_ctx) proof =
  match proof.proof with
  | ForallP(var,pf) -> compile_hol_proof (var::ty_ctx) te_ctx pf_ctx pf
  | Proof(pf) -> compile_hol__proof ty_ctx te_ctx pf_ctx pf._proof
  | RewriteF(ctx,rw,pt) ->
    let eq_proof = (compile_ctx_proof (base_proof rw) ctx pt.term).prf in
    let proof' = compile_hol_proof ty_ctx te_ctx pf_ctx pt in
    OT.mk_eqMp proof' eq_proof


let compile_hol_const name ty term =
  match term with
  | None -> ()
  | Some term ->
    let term' = compile_hol_term [] [] term in
    OT.mk_const (compile_hol_name name) term'

let compile_hol_TyOp name tys = ()

let compile_hol_axiom name hyp term =
  let term' = compile_hol_term [] [] term in
  let hyp' = OT.mk_hyp hyp in
  let thm = OT.mk_axiom hyp' term' in
  OT.mk_thm (compile_hol_name name) term' hyp' thm

let compile_hol_thm name term proof_op =
  match proof_op with
  | None -> compile_hol_axiom name [] term
  | Some proof ->
    let name' = compile_hol_name name in
    OT.mk_thm name'
      (compile_hol_term [] [] term) (OT.mk_hyp []) (compile_hol_proof [] [] [] proof)


let compile_hol_obj (obj:obj) =
  match obj with
  | Cst(name,ty,term) -> compile_hol_const name ty term
  | TyOp(name,tys) -> compile_hol_TyOp name tys
  | Thm(name,term, proof_op) -> compile_hol_thm name term proof_op
