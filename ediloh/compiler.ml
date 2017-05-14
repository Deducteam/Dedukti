open Basic
open Term


type ty_ctx = Basic.ident list

type term_ctx = (Basic.ident * Ast.ty) list

type ty_subst = (Basic.ident * Ast._ty) list

type compile_type_err =
  | FreeTypeVariable of ident
  | TypeError of term

type compile_term_err =
  | FreeTermVariable of ident
  | PolymorphicType of ident * Ast.ty
  | UntypedLambda of term
  | TypeSubstitution of Ast._ty * ty_subst
  | MissingPolymorphicArgument of Ast.ty
  | TermError of term

type compiler_err =
  | DeclarationError of loc * ident * term
  | CompileTypeError of compile_type_err
  | CompileTermError of compile_term_err

exception CompileErr of compiler_err

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

let mk_name md id = [md],id

let rec apply_subst_ty (subst:ty_subst) (ty:Ast.ty) : Ast._ty =
  match ty with
  | Ast.ForallK(var, ty') ->
    assert(List.mem_assoc var subst);
    apply_subst_ty subst ty'
  | Ast.Type(ty') -> apply_subst__ty subst ty'

and apply_subst__ty (subst:ty_subst) (ty:Ast._ty) : Ast._ty =
  match ty with
  | Ast.VarTy(var) ->
    if List.mem_assoc var subst then
      List.assoc var subst
    else
      raise(CompileErr(CompileTermError(TypeSubstitution(ty, subst))))
  | Ast.Arrow(tyl,tyr) ->
    let tyl' = apply_subst__ty subst tyl in
    let tyr' = apply_subst__ty subst tyr in
    Ast.Arrow(tyl',tyr')
  | Ast.OpType(name,tys) ->
    Ast.OpType(name, List.map (apply_subst__ty subst) tys)
  | Ast.Bool -> Ast.Bool

let rec poly_var_of_ty (ty:Ast.ty) : ident list =
  match ty with
  | Ast.Type _ -> []
  | Ast.ForallK(id, ty') ->
    let vars = poly_var_of_ty ty' in
    id::vars

let rec compile_tyOp ty_ctx md id args =
  let args' = List.map (compile__type ty_ctx) args in
  Ast.OpType(mk_name md id, args')

and compile_type (ty_ctx:ty_ctx) (ty:term) : Ast.ty =
  match ty with
  | Term.App(c, Term.Lam(_, var, _, ty), []) when is_hol_const hol_forall_kind_type c ->
    let ty' = compile_type (var::ty_ctx) ty in
    Ast.ForallK(var, ty')
  | _ -> Ast.Type (compile__type ty_ctx ty)

and compile__type (ty_ctx:ty_ctx) (ty:term) : Ast._ty =
  match ty with
  | Term.Const(_,md,id) when is_hol_const hol_prop ty -> Ast.Bool
  | Term.App(c,left,[right]) when is_hol_const hol_arrow c ->
    let left' = compile__type ty_ctx left in
    let right' = compile__type ty_ctx right in
    Ast.Arrow(left',right')
  | Term.App(Term.Const(_,md,id),a,args) -> compile_tyOp ty_ctx md id (a::args)
  | Term.Const(_,md,id) -> compile_tyOp ty_ctx md id []
  | Term.DB(_,var,_) ->
    if List.mem var ty_ctx then
      Ast.VarTy(var)
    else
      raise (CompileErr(CompileTypeError(FreeTypeVariable(var))))
  | _ ->
    raise (CompileErr(CompileTypeError(TypeError(ty))))

let ty_of_const lc md id =
    match Env.get_type lc md id with
    | OK ty -> ty
    | Err er -> Errors.fail_signature_error er

let rec compile__term (ty_ctx:ty_ctx) (te_ctx:term_ctx) (te:term) : Ast._term =
  let lookup_ty var =
    if List.mem_assoc var te_ctx then
      List.assoc var te_ctx
    else
      raise (CompileErr(CompileTermError(FreeTermVariable(var))))
  in
  match te with
  | Term.App(cst, ty, [Term.Lam(_,id, Some tyvar, te)]) when is_hol_const hol_forall cst ->
    assert (Term.term_eq ty tyvar);
    let te' = compile__term ty_ctx te_ctx te in
    let ty' = compile__type ty_ctx ty in
    Ast.Forall(id, ty', te')
  | Term.App(cst, tel, [ter]) when is_hol_const hol_impl cst ->
    let tel' = compile__term ty_ctx te_ctx tel in
    let ter' = compile__term ty_ctx te_ctx ter in
    Ast.Impl(tel',ter')
  | Term.Const(lc,md,id) ->
    let ty = ty_of_const lc md id in
    let ty' = compile__type ty_ctx ty in
    Ast.Const(mk_name md id, ty', [])
  | Term.DB(_,var,_) ->
    let ty = lookup_ty var in
    let ty' =
      match ty with
      | Ast.ForallK(var,te) -> raise(CompileErr(CompileTermError(PolymorphicType(var,ty))))
      | Ast.Type(ty) -> ty
    in
    Ast.VarTerm(var,ty', [])
  | Term.Lam(_,id, Some tyvar, te) ->
    let ty = compile_type ty_ctx tyvar in
    let te' = compile__term ty_ctx ((id,ty)::te_ctx) te in
    Ast.Lam(id,ty, te')
  | Term.Lam(_, _, None, _) ->
    raise(CompileErr(CompileTermError(UntypedLambda(te))))
  | Term.App(Term.Const(lc,md,id),a,args) ->
    let ty = ty_of_const lc md id in
    let ty' = compile_type ty_ctx ty in
    let ty'', args' = _ty_of_ty ty_ctx te_ctx ty' (a::args) in
    Ast.Const(mk_name md id, ty'', args')
  | Term.App(Term.DB(_,var,_),a,args) ->
    let ty = lookup_ty var in
    let ty', args' = _ty_of_ty ty_ctx te_ctx ty (a::args) in
    Ast.VarTerm(var, ty', args')
  | _ -> raise(CompileErr(CompileTermError(TermError(te))))

and _ty_of_ty (ty_ctx:ty_ctx) (te_ctx:term_ctx) (ty:Ast.ty) (args:Term.term list)
  : Ast._ty * Ast._term list =
  let rec split l n =
    if n = 0 then
      [],l
    else
      match l with
      | [] -> raise(CompileErr(CompileTermError(MissingPolymorphicArgument(ty))))
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


and compile_term (ty_ctx:ty_ctx) (te_ctx:term_ctx) (te:term) : Ast.term =
  match te with
  | Term.App(cst, Term.Lam(_,x, Some ty, te'), []) when is_hol_const hol_forall_kind_prop cst ->
    assert (is_hol_const hol_type ty);
    compile_term (x::ty_ctx) (te_ctx) ty
  | _ -> Ast.Term (compile__term ty_ctx te_ctx te)

let compile_declaration (lc:loc) (id:ident) (te:term) : Ast.obj =
  let md = Env.get_name () in
  match te with
  | Term.App(cst,a,[]) when is_hol_const hol_eta cst ->
    Ast.Cst(mk_name md id, compile_type [] a, None)
  | Term.App(cst,a,[]) when is_hol_const hol_eps cst ->
    Ast.Thm(mk_name md id, compile_term [] [] a, None)
  | Term.Const(_,md,id) when is_hol_const hol_type te ->
    Ast.TyOp(mk_name md id, [])
  | _ -> raise (CompileErr(DeclarationError(lc,id,te)))


let mk_prelude (lc:loc) (id:ident) =
  Env.init id

let mk_declaration (lc:loc) (id:ident) (te:term) =
  match Env.declare_constant lc id te with
  | OK () -> ()
  | Err er -> Errors.fail_env_error er

let mk_definable (lc:loc) (id:ident) (te:term) =
  match Env.declare_definable lc id te with
  | OK () -> ()
  | Err er -> Errors.fail_env_error er

let mk_definition (lc:loc) (id:ident) (pty:term option) (te:term) =
  match Env.define lc id te pty with
  | OK () -> ()
  | Err er -> Errors.fail_env_error er

let mk_opaque (lc:loc) (id:ident) (pty:term option) (te:term) =
  match Env.define_op lc id te pty with
  | OK () -> ()
  | Err er -> Errors.fail_env_error er

let mk_rules (rules:Rule.rule list) =
  match Env.add_rules rules with
  | OK _ -> ()
  | Err er -> Errors.fail_env_error er

let mk_command (lc:loc) (cmd:Cmd.command) =
  Cmd.mk_command lc cmd

let mk_ending () =
  if not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ());
