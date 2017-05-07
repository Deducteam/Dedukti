open Basic
open Openstt

open OpenTheory

(* Brendregt convention is used *)
(* ********************************* *)

let counter = ref 0

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

type subst = (Basic.ident * Term.term) list

let const_env:(Basic.ident * (Basic.ident list * Term.term)) list ref = ref []

let print_const_env out const_env = List.iter ( fun (id, (poly,term)) -> Printf.fprintf out "%a\n" Basic.pp_ident id) const_env

let lookup_const md id =
  if List.mem_assoc id !const_env then
    List.assoc id !const_env
  else
    Errors.fail dloc "The constant %a is unknown. List of known constants:\n%a" Basic.pp_ident id  print_const_env !const_env


let type_env:(Basic.ident) list ref = ref []

let proof_env:((Basic.ident) * (term obj * thm obj)) list ref = ref []

let definition_env = ref []

let is_hol_sort t = is_hol_const hol_type t

let is_hol_eta t = is_hol_const hol_eta t

let is_hol_arrow t = is_hol_const hol_arrow t

let is_hol_forall t = is_hol_const hol_forall t

let is_hol_impl t = is_hol_const hol_impl t

let is_hol_prop t = is_hol_const hol_prop t

let is_hol_eps t = is_hol_const hol_eps t

let is_hol_forall_kind_type t = is_hol_const hol_forall_kind_type t

let is_hol_forall_kind_prop t = is_hol_const hol_forall_kind_prop t

let is_hol_type t =
  match t with
  | Term.App(c, ty, _) when is_hol_eta c -> true
  | _ -> false

let extract_hol_type t =
  match t with
  | Term.App(c, ty, _) when is_hol_eta c -> ty
  | _ -> Errors.fail dloc "The term %a should be an application starting with hol.eta" Term.pp_term t

let is_hol_thm t =
  match t with
  | Term.App(c, ty, _) when is_hol_eps c -> true
  | _ -> false

let extract_hol_thm t =
  match t with
  | Term.App(c, ty, _) when is_hol_eps c -> ty
  | _ -> Errors.fail dloc "The term %a should be an application starting with hol.eps" Term.pp_term t

let _ =
  let id = hstring "leibniz" in
  let ty =
    match Env.get_type dloc (hstring "hol") id with
    | OK ty -> extract_hol_type ty
    | Err err -> Format.eprintf "the constant leibniz should be defined in hol.dk";
      Errors.fail_signature_error err
  in
  let vars = [hstring "A"] in
  const_env := (id,(vars,ty))::!const_env


let apply_poly env ty =
  let rec apply_poly env term bool =
    match term with
    | Term.Kind -> assert false
    | Term.DB(_,id,n) -> List.assoc id env
    | Term.Lam(_,_,_,_) -> Errors.fail dloc "Lambda in types (apply poly)"
    | Term.App(f,Term.Lam(_,x, pty, te), []) when is_hol_forall_kind_type f ->
      if bool then
        apply_poly env te true
      else
        Errors.fail dloc "The type %a is not a polymorphic prenex type" Term.pp_term ty
    | Term.App(f,a,args) ->
      Term.mk_App (apply_poly env f false) (apply_poly env a false)
        (List.map (fun x -> apply_poly env x false) args)
    | _ -> term
  in
  match env with
  | [] -> ty
  | _ -> (apply_poly env ty true)



let extract_poly_vars t =
  let rec extract_poly_vars t =
    match t with
    | Term.App(c, Term.Lam(_,x, _, ty), []) when is_hol_forall_kind_type c ->
      x::(extract_poly_vars ty)
    | _ -> []
  in
  extract_poly_vars t

let instr_of_type ty =
  let rec instr_of_tyOp md id args =
    let argsz = List.map instr_of_type args in
    ty_of_tyOp (mk_tyOp (mk_name [] (string_of_ident id))) argsz
  and instr_of_type t =
    match t with
    | Term.Const(_,md,id) when is_hol_prop t -> mk_bool_type
    | Term.App(c,left,[right]) when is_hol_arrow c ->
      mk_arrow_type (instr_of_type left) (instr_of_type right)
    | Term.App(c, Term.Lam(_, var, _, ty), []) when is_hol_forall_kind_type c ->
      instr_of_type ty
    | Term.App(Term.Const(_,md,id),a,args) -> instr_of_tyOp md id (a::args)
    | Term.Const(_,md,id) -> instr_of_tyOp md id []
    | Term.DB(_,var,_) -> mk_varType (mk_name [] (string_of_ident var))
    | _ ->
      Errors.fail dloc "The term %a is not a hol type" Term.pp_term ty
  in
  instr_of_type ty

type env = (Basic.ident * Term.term) list

let is_type_variable env a =
  match a with
  | Term.DB(_,var,_) -> List.mem_assoc var env && is_hol_sort (snd @@ (List.assoc var env))
  | _ -> false

let is_type t =
  match t with
  | Term.Const(_,_,id) -> List.mem id !type_env
  | _ -> false

let rec split n l =
  match n,l with
  | 0,_ -> [],l
  | _,x::t ->
    let l,r = split (n-1) t in
    x::l,r
  | _ -> assert false

let instr_of_thm env thm =
  let lookup env id =
    if List.mem_assoc id env then
      List.assoc id env
    else
      Errors.fail dloc "The variable %a is free in %a" Basic.pp_ident id Term.pp_term thm
  in
  let rec instr_of_thm env t =
    match t with
    | Term.App(c, ty, [te])  when is_hol_forall c ->
      mk_forall_term (instr_of_thm  env te) (instr_of_type ty)
    | Term.App(c, tel, [ter]) when is_hol_impl c ->
      mk_impl_term (instr_of_thm  env tel) (instr_of_thm  env ter)
    | Term.App(c, Term.Lam(_,x, Some tx, ty), []) when is_hol_forall_kind_prop c ->
      instr_of_thm env ty
    | Term.App(Term.Const(_,md,id), a, args) ->
      let poly,ty = lookup_const md id in
      let tys,args = split (List.length poly) (a::args) in
      let tyz = instr_of_type  (apply_poly (List.combine poly tys) ty) in
      let cst = term_of_const (const_of_name (mk_name [] (string_of_ident id))) tyz in
      List.fold_left (fun instr arg -> mk_app_term instr (instr_of_thm  env arg)) cst args
    | Term.App(Term.DB(_,id,_), a, args) ->
      let vars,ty = lookup env id in
      let tys,args = split (List.length vars) (a::args) in
      let tyz = instr_of_type  (apply_poly (List.combine vars tys) ty) in
      let var = mk_var_term (mk_var (mk_name [] (string_of_ident id)) tyz) in
      List.fold_left (fun instr arg -> mk_app_term instr (instr_of_thm  env arg)) var args
    | Term.App(f, a, args) -> Errors.fail dloc "Should not happen except if there is a beta"
    | Term.Lam(_,x,Some ty, te) when is_hol_sort ty ->
      instr_of_thm env te
    | Term.Lam(_,x,Some ty, te) ->
      let ty' = extract_hol_type ty in
      let poly_vars = extract_poly_vars ty' in
      let tz = (instr_of_thm  ((x,(poly_vars,ty'))::env) te) in
      let tyz = instr_of_type  ty' in
      mk_abs_term (mk_var (mk_name [] (string_of_ident x)) tyz) tz
    | Term.Lam(_, _, None, _) ->
      Errors.fail dloc "Lambdas in %a should be typed." Term.pp_term thm
    | Term.DB(_,id,_) ->
      let poly,ty = lookup env id in
      if List.length poly <> 0 then
        Errors.fail dloc "The variable %a should not have a polymorphic type" Basic.pp_ident id
      else
        let tyz = instr_of_type  ty in
        mk_var_term (mk_var (mk_name [] (string_of_ident id)) tyz)
    | Term.Const(_,md,id) ->
      let vars,ty = lookup_const md id in
      term_of_const (const_of_name (mk_name [] (string_of_ident id))) (instr_of_type  ty)
    | _ -> Errors.fail dloc "The following term %a is not handled" Term.pp_term t;
  in
  instr_of_thm env thm

let leb_prefix = "eq"
let leb_sym_prefix = "sym_eq"

let is_beta t =
  match t with
  | Term.App(Term.Const(_,_,id),_,_) -> string_of_ident id = "eq_beta"
  | _ -> false

let is_leibniz t =
  match t with
  | Term.App(Term.Const(_,_,id),a,args) -> Str.(string_match (regexp "eq_\\|sym_eq") (string_of_ident id) 0)
  | _ -> false

type direction = Direct | Indirect

let extract_info_leb md leb = Str.(
    if string_match (regexp "\\(eq_\\|sym_eq_\\)\\(.*\\)") leb 0 then
      let dir = if matched_group 0 leb = "eq" then Direct else Indirect in
      let id = matched_group 2 leb in
      let cst = Term.mk_Const dloc md (hstring id) in
      let def = snd (List.assoc (hstring id) !definition_env) in
      match dir with
      | Direct -> (dir,cst,def,id)
      | Indirect -> (dir,def,cst,id)
    else
      assert false
)

let extract_cons t =
  match t with
  | Term.Const(_,md,id) -> md,id
  | _ -> assert false

let is_forall ctx =
  match ctx with
  | Term.App(f,a,args) -> is_hol_forall f
  | _ -> false

let is_impl ctx =
  match ctx with
  | Term.App(f,a,args) -> is_hol_impl f
  | _ -> false

let is_ctx_var var ctx =
  match ctx with
  | Term.DB(_,id,_) ->  Basic.ident_eq var id
  | _ -> false

let extract_forall ctx =
  match ctx with
  | Term.App(f,a,[Term.Lam(_,x,Some ty,te)]) -> x,ty,te
  | _ -> assert false

let rec plug db holet t =
  match holet with
  | Term.DB(_, id,_) when id = db -> t
  | Term.Lam(l,id,Some ty, te) ->
    Term.mk_Lam l id  (Some(plug db ty t)) (plug db te t)
  | Term.App(f,a,args) ->
    Term.mk_App (plug db f t) (plug db a t) (List.map (fun x -> plug db x t) args)
  | Term.Pi(l,id, ty, te) ->
    Term.mk_Pi l id (plug db ty t) (plug db te t)
  | _ -> holet

let rec proof_of_ctx variable base_proof lctx tl tr ctx proof =
  match ctx with
  | Term.App(f,Term.Lam(_,x,Some ty,te),[]) when is_hol_forall_kind_type f ->
    proof_of_ctx variable base_proof ((x,([], ty))::lctx) tl tr te proof
  | Term.App(f,Term.Lam(_,x,Some ty,te),[]) when is_hol_forall_kind_prop f ->
    proof_of_ctx variable base_proof ((x,([], ty))::lctx) tl tr te proof
  | Term.App(f,a,[Term.Lam(_,var,Some ty,te)]) when is_hol_forall f ->
    let ty = extract_hol_type ty in
    let poly_vars = extract_poly_vars ty in
    let lctx' = (var,(poly_vars,ty))::lctx in
    let termlz,termrz,proof' = proof_of_ctx variable base_proof lctx' tl tr te proof in
    let termlz' = mk_abs_term (mk_var (mk_name [] (string_of_ident var)) (instr_of_type ty)) termlz in
    let termrz' = mk_abs_term (mk_var (mk_name [] (string_of_ident var)) (instr_of_type ty)) termrz in
    termlz',termrz', mk_forall_equal proof' (mk_name [] (string_of_ident var)) termlz termrz  (instr_of_type ty)
  | Term.App(f,left,[right]) when is_hol_impl f ->
    let _,leftz,proofl = proof_of_ctx variable base_proof lctx tl tr left proof in
    let _,rightz,proofr = proof_of_ctx variable base_proof lctx tl tr right proof in
    let leftz',rightz' = instr_of_thm lctx left, instr_of_thm lctx right in
    mk_impl_term leftz' rightz', mk_impl_term leftz rightz, mk_impl_equal proofl proofr leftz' rightz' leftz rightz
  | Term.DB(_,id,_) when Basic.ident_eq id variable ->
    let tlz = instr_of_thm lctx tl in
    let trz = instr_of_thm lctx tr in
    tlz,trz,base_proof
  | Term.App(Term.Const(_,md,id), a, args) ->
    let vars,ty = lookup_const md id in
    let tys,args = split (List.length vars) (a::args) in
    let ty' = instr_of_type (apply_poly (List.combine vars tys) ty) in
    let cst = term_of_const (const_of_name (mk_name [] (string_of_ident id))) ty' in
    List.fold_left (fun (terml,termr,proof) arg ->
        let terml',termr',proof' = proof_of_ctx variable base_proof lctx tl tr arg proof in
        mk_app_term terml terml', mk_app_term termr termr', mk_appThm proof proof') (cst,cst,mk_refl cst) args
  | Term.App(Term.DB(_,id,_) as f, a, args) ->
    let vars,ty = List.assoc id lctx in
    let tys,args = split (List.length vars) (a::args) in
    List.fold_left (fun (terml,termr,proof) arg ->
        let terml',termr',proof' = proof_of_ctx variable base_proof lctx tl tr arg proof in
        mk_app_term terml terml', mk_app_term termr termr', mk_appThm proof proof') (proof_of_ctx variable base_proof lctx tl tr f proof) args
  | Term.App(f,a,args) ->
    List.fold_left (fun (terml,termr,proof) arg ->
        let terml', termr',proof' = proof_of_ctx variable base_proof lctx tl tr arg proof in
        mk_app_term terml terml', mk_app_term termr termr', mk_appThm proof proof') (proof_of_ctx variable base_proof lctx tl tr f proof) (a::args)
  | _ ->
    let ctx' = instr_of_thm lctx ctx in
    ctx',ctx',mk_refl ctx'


let is_typed_beta redex =
  match redex with
  | Term.App(Term.Lam(_,_,Some ty,_),_,[])
    when is_hol_sort ty -> true
  | _ -> false

let beta_reduce redex =
  match redex with
  | Term.App(Term.Lam(_,_,Some ty,te),a,[]) ->
    Subst.subst te a
  | _ -> assert false

let rec instr_of_proof  ctx t =
  match t with
  | Term.Lam(_,x, Some ty, t') when is_hol_sort ty ->
    instr_of_proof ctx t'
  | Term.Lam(_,x, Some ty, t') when is_hol_type ty ->
    let ty' = (instr_of_type  (extract_hol_type ty)) in
    let poly_vars = extract_poly_vars ty in
    let t,thm = instr_of_proof  ((x,(poly_vars,(extract_hol_type ty)))::ctx) t' in
    let term = mk_forall_term (mk_abs_term (mk_var (mk_name [] (string_of_ident x)) ty') t) ty' in
    term, mk_rule_intro_forall (mk_name [] (string_of_ident x)) ty' t thm
  | Term.Lam(_,x, Some ty, t') when is_hol_thm ty ->
    let p = (instr_of_thm ctx (extract_hol_thm ty)) in
    let ty' = extract_hol_thm ty in
    let poly_vars = extract_poly_vars ty' in
    let q,thm = instr_of_proof  ((x,(poly_vars,ty'))::ctx) t' in
    let term = mk_impl_term p q in
    term, mk_rule_intro_impl thm p q
  | Term.DB(_,id,_) ->
    let _,ty' = List.assoc id ctx in
    let instr = instr_of_thm ctx ty' in
    instr, mk_assume instr
  | Term.App(f,redex,[a;args]) when is_beta t ->
    if is_typed_beta redex then
      instr_of_proof  ctx args
    else
      let t,thm = instr_of_proof  ctx args in
      let equality = mk_betaConv (instr_of_thm ctx redex) in
      let redex' = beta_reduce redex in
      begin
        match a with
        | Term.Lam(_,id,Some ty,te) ->
          let ty' = extract_hol_type ty in
          let poly_vars = extract_poly_vars ty' in
          let terml,termr, eq_proof = proof_of_ctx id equality [id,(poly_vars,ty')] redex redex' te thm in
          termr, mk_eqMp thm eq_proof
        | _ -> failwith "not valid context is it eta expanded ?"
      end
  | Term.App(f,a,[args]) when is_leibniz t ->
    let t,thm = instr_of_proof  ctx args in
    let (md,cst) = extract_cons f in
    let (dir,tl,tr,id) = extract_info_leb md (string_of_ident cst) in
    let cst_name = mk_name [] id in
    let equality = thm_of_const_name cst_name in
    let equality =
    begin
      match dir with
      | Direct -> equality
      | Indirect -> mk_sym equality
    end
    in
    begin
      match a with
      | Term.Lam(_,id,Some ty,te) ->
        let ty' = extract_hol_type ty in
        let poly_vars = extract_poly_vars ty' in
        let terml,termr, eq_proof = proof_of_ctx id equality [id,(poly_vars,ty')] tl tr  te thm in
        termr,mk_eqMp thm eq_proof
      | Term.Lam(_,_,None,_) -> failwith "lambda ctx is not typed"
      | _ -> failwith "it is not a context or it is not eta expanded"
    end
  | Term.Const(_,md,id) ->
    List.assoc id !proof_env
  | _ -> (Pp.print_term Format.std_formatter t; failwith "instr_of_proof")

let mk_prelude lc name = Env.init name (* (Examples.test ()) *)

(* Assume that one defines an external type operator id *)
let define_hol_type id = type_env := id::!type_env


let define_hol_const id te = mk_const (mk_name [] (string_of_ident id)) (instr_of_thm [] te)

let define_axiom term =
  let term' = instr_of_thm [] term in
  (*
  mk_thm term' (mk_hyp []) (mk_axiom (mk_hyp []) term') *)
  mk_axiom (mk_hyp []) term'

let define_thm term hyp thm =
  mk_thm term hyp thm

let mk_declaration lc id pty : unit =
  match pty with
  | Term.Const(_,md,id) when is_hol_sort pty ->
    define_hol_type id
  | Term.App(f, a, []) when is_hol_eta f ->
      (* no implementation provided for that constant, then it is external *)
      let pty' = extract_hol_type pty in
      let poly_vars = extract_poly_vars pty' in
      const_env := (id, (poly_vars,pty'))::!const_env
  | Term.App(f, a, []) when is_hol_eps f ->
    let term = extract_hol_thm pty in
    let ax = define_axiom term in
    let term' = instr_of_thm [] term in
    proof_env := (id,(term',ax))::!proof_env
  | _ -> Errors.fail dloc "case not handle: %a" Term.pp_term pty

let add_definition id ty pte =
  definition_env := (id,(ty,pte))::!definition_env

let mk_definable lc id pty : unit = failwith "definable symbol without definition... should not happend"

let mk_definition lc id pty_opt pte : unit =
  let ty =
    match pty_opt with
    | Some ty -> ty
    | None -> failwith "missing type in the definition"
  in
  add_definition id ty pte;
  match ty with
  | Term.App(f, a, []) when is_hol_eta f ->
    begin
      let ty' = extract_hol_type ty in
      let poly_vars = extract_poly_vars ty' in
      const_env := (id, (poly_vars,ty'))::!const_env;
      define_hol_const id pte
    end
  | Term.App(f, a, []) when is_hol_eps f ->
    let _,thm = instr_of_proof [] pte in
    let te = instr_of_thm [] (extract_hol_thm ty) in
    proof_env :=(id,(te,thm))::!proof_env;
    mk_thm (instr_of_thm [] (extract_hol_thm ty)) (mk_hyp []) thm
  | _ -> Errors.fail dloc "case not handle: %a" Term.pp_term ty


let mk_opaque lc id pty_opt pte =
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Rule.Pattern (l,md,id,_) -> (l,md,id)
  | _ -> (dloc,qmark,qmark)

let mk_rules = function
  | [] -> ()
  | ((_,pat,_)::_) as lst ->
    begin
      match Env.add_rules lst with
      | OK _ -> ()
      | Err e -> Errors.fail_env_error e
    end

let mk_command = Cmd.mk_command

let export = ref false

let mk_ending () = ()
