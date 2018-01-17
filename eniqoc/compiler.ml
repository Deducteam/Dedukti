open Basic

type ty_ctx = ident list

let sttfa_module = mk_mident "sttfa"
let sttfa_type = mk_ident "type"
let sttfa_ptype = mk_ident "ptype"
let sttfa_eta = mk_ident "eta"
let sttfa_etap = mk_ident "etap"
let sttfa_p = mk_ident "p"
let sttfa_arrow = mk_ident "arrow"
let sttfa_forall = mk_ident "forall"
let sttfa_leibniz = mk_ident "leibniz"
let sttfa_impl = mk_ident "impl"
let sttfa_prop = mk_ident "bool"
let sttfa_eps = mk_ident "eps"
let sttfa_forall_kind_type = mk_ident "forallK"
let sttfa_forall_kind_prop = mk_ident "forallP"

let logic_module = mk_mident "logic"

let is_sttfa_const c t =
  match t with
  | Term.Const(_, cst) -> name_eq cst (mk_name sttfa_module c)
  | _ -> false

let is_type t =
  match t with
  | Term.App(cst, ty, _) when is_sttfa_const sttfa_eta cst -> true
  | _ -> false

let is_term t =
  match t with
  | Term.App(cst, ty, _) when is_sttfa_const sttfa_eps cst -> true
  | _ -> false

let list_of_sttfa_theorem_with_eq =
  [mk_ident "eq_minus_O"; mk_ident "eq_minus_S_pred"; mk_ident "eq_to_eqb_true"; mk_ident "eq_to_eqb_false"; mk_ident "eq_times_div_minus_mod"; mk_ident "eq_to_bijn"; mk_ident "eq_minus_gcd_aux"; mk_ident "eq_div_O"; mk_ident "eq_minus_gcd"; mk_ident "eq_times_plus_to_congruent"; mk_ident "eq_mod_to_divides"; mk_ident "eq_fact_pi_p"]

let is_delta_rw cst =
  match cst with
  | Term.Const(_,cst) ->
    not (mident_eq (md cst) logic_module) && Str.(string_match (regexp "eq_\\|sym_eq") (string_of_ident (id cst)) 0) && not (List.exists (fun thm -> ident_eq (id cst) thm) list_of_sttfa_theorem_with_eq)
  | _ -> false

let get_infos_of_delta_rw md id = Str.(
    let id = string_of_ident id in
    if string_match (regexp "\\(__eq__\\)\\(.*\\)") id 0 then
      let id = matched_group 2 id in
      let cst = Term.mk_Const Basic.dloc (mk_name md (mk_ident id)) in
      match Env.reduction (Reduction.NSteps 1) cst with
      | OK te -> (mk_ident id),te
      | Err err -> Errors.fail_env_error err
    else
      assert false
)

let rec arguments_needed rw =
  let ty =
    match rw with
    | Term.Const(lc,cst) ->
      begin
        match Env.get_type lc cst with
        | OK ty -> ty
        | Err err -> Errors.fail_signature_error err
      end
    | _ -> assert false
  in
  let rec aux t n =
    match t with
    | Term.App(Term.Const(_,_) as cst,_,_) when is_sttfa_const sttfa_leibniz cst -> n
    | Term.App(Term.Const(_,_) as cst, Term.Lam(_,_,_,te),[])
      when is_sttfa_const sttfa_forall_kind_prop cst -> aux te (n+1)
    | Term.App(Term.Const(_,_) as cst,_,[Term.Lam(_,_,_,te)]) when is_sttfa_const sttfa_forall cst ->
      aux te (n+1)
    | Term.App(Term.Const(_,_) as cst, t', []) when is_sttfa_const sttfa_eps cst -> aux t' n
    | _ -> Format.eprintf "debug: %a@." Pp.print_term rw; assert false
  in
  aux ty 0

let rec compile_term ty_ctx te_ctx te =
  match te with
  | Term.App(c, Term.Lam(_, var, _, ty), []) when is_sttfa_const sttfa_forall_kind_type c ->
    let ty' = compile_term (var::ty_ctx) te_ctx ty in
    Ast.Forall(var, Ast.Type, ty')
  | Term.App(c, a, []) when is_sttfa_const sttfa_p c ->
    compile_term ty_ctx te_ctx a
  | Term.Const(_,cst) when is_sttfa_const sttfa_prop te -> Ast.Prop
  | Term.App(c,left,[right]) when is_sttfa_const sttfa_arrow c ->
    let left' = compile_term ty_ctx te_ctx left in
    let right' = compile_term ty_ctx te_ctx right in
    Ast.Impl(left',right')
  | Term.App(cst, Term.Lam(_,x, Some ty, te'), []) when is_sttfa_const sttfa_forall_kind_prop cst ->
    assert (is_sttfa_const sttfa_type ty);
    Ast.Forall(x, Ast.Type, compile_term (x::ty_ctx) (te_ctx) te')
  | Term.App(cst, ty, [Term.Lam(_,id, Some tyvar, te)]) when is_sttfa_const sttfa_forall cst ->
    let ty' = compile_term ty_ctx te_ctx ty in
    let te' = compile_term ty_ctx ((id, ty')::te_ctx) te in
    Ast.Forall(id, ty', te')
  | Term.App(cst, tel, [ter]) when is_sttfa_const sttfa_impl cst ->
    let tel' = compile_term ty_ctx te_ctx tel in
    let ter' = compile_term ty_ctx te_ctx ter in
    Ast.Impl(tel',ter')
  | Term.Const(lc,cst) ->
    Ast.Const(cst)
  | Term.DB(_,var,n) ->
    Ast.Var(var)
  | Term.Lam(_,id, Some cst, te) when is_sttfa_const sttfa_type cst ->
    Ast.Lam(id, Ast.Type, compile_term (id::ty_ctx) te_ctx te)
  | Term.Lam(_,id, Some tyvar, te) ->
    let _ty' = compile_wrapped_type ty_ctx te_ctx tyvar in
    let te' = compile_term ty_ctx ((id,_ty')::te_ctx) te in
    Ast.Lam(id,_ty', te')
  | Term.Lam(_, _, None, _) -> failwith "lambda untyped are not supported"
  | Term.App(f,a,args) ->
    let f' = compile_term ty_ctx te_ctx f in
    let args' = List.map (fun x -> compile_term ty_ctx te_ctx x) (a::args) in
    List.fold_left (fun t a -> Ast.App(t,a)) f' args'
  | _ ->  Format.eprintf "debug: %a@." Pp.print_term te; assert false

and compile_wrapped_type (ty_ctx:ty_ctx) te_ctx (ty:Term.term)  =
  match ty with
  | Term.App(cst, Term.App(c,a,[]), []) when (is_sttfa_const sttfa_etap cst && is_sttfa_const sttfa_p c)  ->
    compile_term ty_ctx te_ctx a
  | Term.App(cst, a, []) when is_sttfa_const sttfa_eps cst ->
    compile_term ty_ctx te_ctx a
  | _ -> Format.printf "debug:%a@." Pp.print_term ty; assert false

let compile_declaration name ty =
      match ty with
    | Term.App(cst,a,[]) when is_sttfa_const sttfa_etap cst ->
      Ast.Parameter(name, compile_term [] [] a)
    | Term.App(cst,a,[]) when is_sttfa_const sttfa_eps cst ->
      Ast.Axiom(name, compile_term [] [] a)
    | Term.Const(_,_) when is_sttfa_const sttfa_type ty ->
      Ast.Parameter(name, Ast.Type)
    | _ -> assert false

let compile_definition name ty term =
  match ty with
  | Term.App(cst,a,[]) when is_sttfa_const sttfa_etap cst ->
    Ast.Constant(name, compile_term [] [] a, compile_term [] [] term)
  | Term.App(cst,a,[]) when is_sttfa_const sttfa_eps cst ->
    Ast.Theorem(name, compile_term [] [] a, compile_term [] [] term)
  | _ -> assert false

let ast = ref {Ast.name="";
               Ast.obj=[];
              }

let init_ast name =
  ast :=
    {
      Ast.name=string_of_ident name;
      Ast.obj=[];
    }

let get_ast () =
  let open Ast in
  let ast = !ast in
  {ast with obj = List.rev ast.obj}

let add_declaration decl =
  let open Ast in
  let ast' = !ast in
  ast := {ast' with obj = Declaration(decl)::ast'.obj}

let add_definition defn =
  let open Ast in
  let ast' = !ast in
  ast := {ast' with obj = Definition(defn)::ast'.obj}
