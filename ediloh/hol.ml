open Basic

type name = ident list * ident

type _ty =
  | VarTy of ident
  | Arrow of _ty * _ty
  | OpType of name * _ty list
  | Bool

type ty =
  | ForallK of ident * ty
  | Type of _ty

type _term =
  | Forall of ident * _ty * _term
  | Impl of _term * _term
  | VarTerm of ident * _ty * _term list
  | Const of name * _ty * _term list
  | Lam of ident * ty * _term

type term =
  | ForallT of ident * term
  | Term of _term

type proof

type obj =
  | Cst of name * ty * term option
  | TyOp of name * _ty list
  | Thm of name * term * proof option

type ty_ctx = Basic.ident list

type term_ctx = (Basic.ident * ty) list

type ty_subst = (Basic.ident * _ty) list

type decl = loc * ident * Term.term

type compile_type_err =
  | FreeTypeVariable of ident
  | TypeError of Term.term

type compile_term_err =
  | FreeTermVariable of ident
  | PolymorphicType of ident * ty
  | UntypedLambda of Term.term
  | TypeSubstitution of _ty * ty_subst
  | MissingPolymorphicArgument of ty
  | TermError of Term.term

type compile_decl_err =
  | DeclarationError of decl
  | DeclarationTypeError of compile_type_err * decl
  | DeclarationTermError of compile_term_err * decl

exception CompileTypeError of compile_type_err

exception CompileTermError of compile_term_err

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

let rec apply_subst_ty (subst:ty_subst) (ty:ty) : _ty =
  match ty with
  | ForallK(var, ty') ->
    assert(List.mem_assoc var subst);
    apply_subst_ty subst ty'
  | Type(ty') -> apply_subst__ty subst ty'

and apply_subst__ty (subst:ty_subst) (ty:_ty) : _ty =
  match ty with
  | VarTy(var) ->
    if List.mem_assoc var subst then
      List.assoc var subst
    else
      raise(CompileTermError(TypeSubstitution(ty, subst)))
  | Arrow(tyl,tyr) ->
    let tyl' = apply_subst__ty subst tyl in
    let tyr' = apply_subst__ty subst tyr in
    Arrow(tyl',tyr')
  | OpType(name,tys) ->
    OpType(name, List.map (apply_subst__ty subst) tys)
  | Bool -> Bool

let rec poly_var_of_ty (ty:ty) : ident list =
  match ty with
  | Type _ -> []
  | ForallK(id, ty') ->
    let vars = poly_var_of_ty ty' in
    id::vars

let mk_name md id = [md],id

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
      raise (CompileTypeError(FreeTypeVariable(var)))
  | _ ->
    raise (CompileTypeError(TypeError(ty)))

let ty_of_const lc md id =
    match Env.get_type lc md id with
    | OK ty -> ty
    | Err er -> Errors.fail_signature_error er

let rec compile__term (ty_ctx:ty_ctx) (te_ctx:term_ctx) (te:Term.term) : _term =
  let lookup_ty var =
    if List.mem_assoc var te_ctx then
      List.assoc var te_ctx
    else
      raise (CompileTermError(FreeTermVariable(var)))
  in
  match te with
  | Term.App(cst, ty, [Term.Lam(_,id, Some tyvar, te)]) when is_hol_const hol_forall cst ->
    assert (Term.term_eq ty tyvar);
    let te' = compile__term ty_ctx te_ctx te in
    let ty' = compile__type ty_ctx ty in
    Forall(id, ty', te')
  | Term.App(cst, tel, [ter]) when is_hol_const hol_impl cst ->
    let tel' = compile__term ty_ctx te_ctx tel in
    let ter' = compile__term ty_ctx te_ctx ter in
    Impl(tel',ter')
  | Term.Const(lc,md,id) ->
    let ty = ty_of_const lc md id in
    let ty' = compile__type ty_ctx ty in
    Const(mk_name md id, ty', [])
  | Term.DB(_,var,_) ->
    let ty = lookup_ty var in
    let ty' =
      match ty with
      | ForallK(var,te) -> raise(CompileTermError(PolymorphicType(var,ty)))
      | Type(ty) -> ty
    in
    VarTerm(var,ty', [])
  | Term.Lam(_,id, Some tyvar, te) ->
    let ty = compile_type ty_ctx tyvar in
    let te' = compile__term ty_ctx ((id,ty)::te_ctx) te in
    Lam(id,ty, te')
  | Term.Lam(_, _, None, _) ->
    raise(CompileTermError(UntypedLambda(te)))
  | Term.App(Term.Const(lc,md,id),a,args) ->
    let ty = ty_of_const lc md id in
    let ty' = compile_type ty_ctx ty in
    let ty'', args' = _ty_of_ty ty_ctx te_ctx ty' (a::args) in
    Const(mk_name md id, ty'', args')
  | Term.App(Term.DB(_,var,_),a,args) ->
    let ty = lookup_ty var in
    let ty', args' = _ty_of_ty ty_ctx te_ctx ty (a::args) in
    VarTerm(var, ty', args')
  | _ -> raise(CompileTermError(TermError(te)))

and _ty_of_ty (ty_ctx:ty_ctx) (te_ctx:term_ctx) (ty:ty) (args:Term.term list)
  : _ty * _term list =
  let rec split l n =
    if n = 0 then
      [],l
    else
      match l with
      | [] -> raise(CompileTermError(MissingPolymorphicArgument(ty)))
      | x::t ->
        let poly,args = split t (n-1) in
        x::poly,args
  in
  let poly_vars = poly_var_of_ty ty in
  let n = List.length poly_vars in
  let poly_args,args = split args n in
  let poly_args' = List.map (compile__type ty_ctx) poly_args in
  let subst = List.combine poly_vars poly_args' in
  let _ty = apply_subst_ty subst ty in
  let args' = List.map (compile__term ty_ctx te_ctx) args in
  _ty, args'

and compile_term (ty_ctx:ty_ctx) (te_ctx:term_ctx) (te:Term.term) : term =
  match te with
  | Term.App(cst, Term.Lam(_,x, Some ty, te'), []) when is_hol_const hol_forall_kind_prop cst ->
    assert (is_hol_const hol_type ty);
    compile_term (x::ty_ctx) (te_ctx) ty
  | _ -> Term (compile__term ty_ctx te_ctx te)

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

let fail_compile_term (err:compile_term_err) (decl:decl) = failwith "todo"

let fail_compile_type (err:compile_type_err) (decl:decl) = failwith "todo"

let fail_compile_declaration (err:compile_decl_err) =
  match err with
  | DeclarationError(lc,id,te) ->
    Errors.fail lc "Error while compiling the declaration '%a'. It seems that his type is not recognized by the compiler: %a@." Basic.print_ident id Pp.print_term te
  | DeclarationTermError(err,decl) ->
    fail_compile_term err decl
  | DeclarationTypeError(err,decl) ->
    fail_compile_type err decl
