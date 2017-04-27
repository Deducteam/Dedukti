open Basic
open Openstt

open OpenTheory

let print_term t = Pp.print_term Format.std_formatter t;Format.printf "@."

(* ********************************* *)

let counter = ref 0

let hol_module = hstring "hol"
let hol_sort = hstring "type"
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

let type_env:(Basic.ident) list ref = ref []

let proof_env:((Basic.ident) * (term obj * thm obj)) list ref = ref []

let is_hol_sort t = is_hol_const hol_sort t

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
  | Term.App (c, _, _) -> is_hol_eta c
  | _ -> false

let is_hol_proof t =
  match t with
  | Term.App (c, _, _) -> is_hol_eps c
  | _ -> false

let extract_type t =
  match t with
  | Term.App(c, ty, _) when is_hol_eta c -> ty
  | _ -> assert false

let extract_proof t =
  match t with
  | Term.App(c, ty, _) when is_hol_eps c -> ty
  | _ -> assert false

let extract_term t =
  match t with
  | Term.App(c, ty, _) when is_hol_eps c -> ty
  | _ -> assert false



let _ =
  let id = hstring "leibniz" in
  let ty =
    match Env.get_type dloc (hstring "hol") id with
    | OK ty -> extract_type ty
    | Err err -> failwith "the constant leibniz should be defined in hol.dk"
  in
  let vars = [hstring "A"] in
  const_env := (id,(vars,ty))::!const_env


let apply_poly env term =
  let rec apply_poly env term bool =
    match term with
    | Term.Kind -> assert false
    | Term.DB(_,id,n) -> List.assoc id env
    | Term.Lam(_,_,_,_) -> failwith "not handled poly"
    | Term.App(f,Term.Lam(_,x, pty, te), []) when is_hol_forall_kind_type f ->
      if bool then
        apply_poly env te true
      else
        assert false
    | Term.App(f,a,args) ->
      Term.mk_App (apply_poly env f false) (apply_poly env a false) (List.map (fun x -> apply_poly env x false) args)
    | _ -> term
  in
  match env with
  | [] -> term
  | _ -> apply_poly env term true

let rec extract_poly_vars t =
  match t with
  | Term.App(c, Term.Lam(_,x, _, ty), []) when is_hol_forall_kind_type c ->
    x::(extract_poly_vars ty)
  | _ -> []

let rec instr_of_type t =
  match t with
  | Term.Const(_,m,id) when is_hol_prop t ->
    mk_bool_type
  | Term.Const(_,m, id) ->
    (* ASSUMPTION : every type operator is of arity 0 *)
    ty_of_tyOp (mk_tyOp (mk_name [] (string_of_ident id))) []
  (* ASSUMPTION : no clash in names and id should start with a majuscule *)
  | Term.DB(_,id,i) ->
    mk_varType (mk_name [] (string_of_ident id))
  | Term.App(c, tyl, [tyr]) when is_hol_arrow c ->
    mk_arrow_type (instr_of_type tyl) (instr_of_type tyr)
  | Term.App(c, Term.Lam(_,x, _, ty), []) when is_hol_forall_kind_type c ->
    instr_of_type ty
  | _ -> Pp.print_term Format.std_formatter t; failwith "todo type"



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




(* TODO add a dictionnary *)
let rec instr_of_term env t =
  match t with
  | Term.App(c, ty, [te])  when is_hol_forall c ->
    mk_forall_term (instr_of_term env te) (instr_of_type ty)
  | Term.App(c, tel, [ter]) when is_hol_impl c ->
    mk_impl_term (instr_of_term env tel) (instr_of_term env ter)
  | Term.App(c, Term.Lam(_,x, Some tx, ty), []) when is_hol_forall_kind_prop c ->
    instr_of_term ((x,([],tx))::env) ty
  | Term.App(Term.Const(_,_,id), a, args) ->
    let vars,ty = List.assoc id !const_env in
    let tys,args = split (List.length vars) (a::args) in
    let ty' = instr_of_type (apply_poly (List.combine vars tys) ty) in
    let cst = term_of_const (const_of_name (mk_name [] (string_of_ident id))) ty' in
    List.fold_left (fun instr arg -> mk_app_term instr (instr_of_term env arg)) cst args
  | Term.App(Term.DB(_,id,_), a, args) ->
    let vars,ty = List.assoc id env in
    let tys,args = split (List.length vars) (a::args) in
    let ty' = instr_of_type (apply_poly (List.combine vars tys) ty) in
    let var = mk_var_term (mk_var (mk_name [] (string_of_ident id)) ty') in
    List.fold_left (fun instr arg -> mk_app_term instr (instr_of_term env arg)) var args
  | Term.App(f, a, args) ->
    List.fold_left (fun instr a -> if is_type_variable env a then instr else mk_app_term instr (instr_of_term env a))
      (instr_of_term env f) (a::args)
  | Term.Lam(_,x,Some tx, t') when is_hol_sort tx ->
    instr_of_term ((x,([],tx))::env) t'
  | Term.Lam(_,x,Some tx, t') ->
    let tx' = extract_type tx in
    let poly_vars = extract_poly_vars tx' in
    let it = (instr_of_term ((x,(poly_vars,tx'))::env) t') in
    let ty = instr_of_type tx' in
    mk_abs_term (mk_var (mk_name [] (string_of_ident x)) ty) it
  | Term.Lam(_, _, None, _) -> failwith "every lambda should be typed"
  | Term.DB(_,id,i) ->
    (* ASSUMPTION : no clash in names variable as in \x\x.x*)
    let _,itx = (List.assoc id env) in
    let itx = instr_of_type itx in
    mk_var_term (mk_var (mk_name [] (string_of_ident id)) itx)
  | Term.Const(_,_,id) ->
    let vars,ty = List.assoc id !const_env in
    term_of_const (const_of_name (mk_name [] (string_of_ident id))) (instr_of_type ty)
  | _ -> Pp.print_term Format.std_formatter t; failwith "todo term"


let leb_prefix = "eq"
let leb_sym_prefix = "sym_eq"

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
      match Env.one cst with
      | OK None -> assert false
      | OK (Some def) ->
        begin
          match dir with
          | Direct -> (dir,cst,def,id)
          | Indirect -> (dir,def,cst,id)
        end
      | Err er -> Errors.fail_env_error er
    else
      assert false
)

let extract_cons t =
  match t with
  | Term.Const(_,md,id) -> md,id
  | _ -> assert false


let is_forall_kind ctx =
  match ctx with
  | Term.App(f,a,args) -> is_hol_forall_kind_type f
  | _ -> false

let is_forall_prop ctx =
  match ctx with
  | Term.App(f,a,args) -> is_hol_forall_kind_prop f
  | _ -> false

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

let extract_forall_kind ctx =
  match ctx with
  | Term.App(f,Term.Lam(_,x,Some ty,te),[]) -> x,ty,te
  | _ -> assert false

let extract_forall_prop ctx =
  match ctx with
  | Term.App(f,Term.Lam(_,x,Some ty,te),[]) -> x,ty,te
  | _ -> assert false

let extract_forall ctx =
  match ctx with
  | Term.App(f,a,[Term.Lam(_,x,Some ty,te)]) -> x,ty,te
  | _ -> assert false

let extract_impl ctx =
  match ctx with
  | Term.App(f,left,[right]) -> left,right
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
  if is_forall_kind ctx then
    let x,ty,te = extract_forall_kind ctx in
    proof_of_ctx variable base_proof ((x,([],ty))::lctx) tl tr te proof
  else
  if is_forall_prop ctx then
    let x,ty,te = extract_forall_prop ctx in
    proof_of_ctx variable base_proof ((x,([],ty))::lctx) tl tr te proof
  else
  if is_forall ctx then
    let var,ty,te = extract_forall ctx in
    let ty = extract_type ty in
    let poly_vars = extract_poly_vars ty in
    let lctx' = (var,(poly_vars,ty))::lctx in
    let terml,termr,proof' = proof_of_ctx variable base_proof lctx' tl tr te proof in
    let terml' = mk_abs_term (mk_var (mk_name [] (string_of_ident var)) (instr_of_type ty)) terml in
    let termr' = mk_abs_term (mk_var (mk_name [] (string_of_ident var)) (instr_of_type ty)) termr in
    terml',termr', mk_forall_equal proof' (mk_name [] (string_of_ident var)) terml termr  (instr_of_type ty)
  else
  if is_impl ctx then
    let left,right = extract_impl ctx in
    let _,left',proofl = proof_of_ctx variable base_proof lctx tl tr left proof in
    let _,right',proofr = proof_of_ctx variable base_proof lctx tl tr right proof in
    let left,right = instr_of_term lctx left, instr_of_term lctx right in
    mk_impl_term left right, mk_impl_term left' right',mk_impl_equal proofl proofr left right left' right'
  else
  if is_ctx_var variable ctx then
    let tl' = instr_of_term lctx tl in
    let tr' = instr_of_term lctx tr in
    tl',tr',base_proof
  else
    match ctx with
(*
    | Term.App(Term.Const(_,_,id), a, args) ->
      let vars,ty = List.assoc id !const_env in
      let tys,args = split (List.length vars) (a::args) in
      let ty' = instr_of_type (apply_poly (List.combine vars tys) ty) in
      let cst = term_of_const (const_of_name (mk_name [] (string_of_ident id))) ty' in
      List.fold_left (fun instr arg -> mk_app_term instr (instr_of_term env arg)) cst args *)
    | Term.App(Term.DB(_,id,_) as f, a, args) ->
      let vars,ty = List.assoc id lctx in
      let tys,args = split (List.length vars) (a::args) in
      let ty' = instr_of_type (apply_poly (List.combine vars tys) ty) in
      let var = mk_var_term (mk_var (mk_name [] (string_of_ident id)) ty') in
      List.fold_left (fun (terml,termr,proof) arg ->
          let terml',termr',proof' = proof_of_ctx variable base_proof lctx tl tr arg proof in
          mk_app_term terml terml', mk_app_term termr termr', mk_appThm proof proof') (proof_of_ctx variable base_proof lctx tl tr f proof) args
    | Term.App(f,a,args) ->
      List.fold_left (fun (terml,termr,proof) arg ->
          let terml', termr',proof' = proof_of_ctx variable base_proof lctx tl tr arg proof in
          mk_app_term terml terml', mk_app_term termr termr', mk_appThm proof proof') (proof_of_ctx variable base_proof lctx tl tr f proof) (a::args)
    | _ ->
      let ctx' = instr_of_term lctx ctx in
      ctx',ctx',mk_refl ctx'




let rec instr_of_proof ctx t =
  match t with
  | Term.Lam(_,x, Some ty, t') when is_hol_sort ty ->
    instr_of_proof ctx t'
  | Term.Lam(_,x, Some ty, t') when is_hol_type ty ->
    let ty' = (instr_of_type (extract_type ty)) in
    let poly_vars = extract_poly_vars ty in
    let t,thm = instr_of_proof ((x,(poly_vars,(extract_type ty)))::ctx) t' in
    let term = mk_forall_term (mk_abs_term (mk_var (mk_name [] (string_of_ident x)) ty') t) ty' in
    term, mk_rule_intro_forall (mk_name [] (string_of_ident x)) ty' t thm
  | Term.Lam(_,x, Some ty, t') when is_hol_proof ty ->
    let p = (instr_of_term ctx (extract_proof ty)) in
    let ty' = extract_proof ty in
    let poly_vars = extract_poly_vars ty' in
    let q,thm = instr_of_proof ((x,(poly_vars,ty'))::ctx) t' in
    let term = mk_impl_term p q in
    term, mk_rule_intro_impl thm p q
  | Term.DB(_,id,_) ->
    let _,ty' = List.assoc id ctx in
    let instr = instr_of_term ctx ty' in
    instr, mk_assume instr
  | Term.App(f,a,[args]) when is_leibniz t ->
    let t,thm = instr_of_proof ctx args in
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
        let ty' = extract_type ty in
        let poly_vars = extract_poly_vars ty' in
        let terml,termr, eq_proof = proof_of_ctx id equality [id,(poly_vars,ty')] tl tr  te thm in
        termr,mk_eqMp thm eq_proof
      | Term.Lam(_,_,None,_) -> failwith "lambda ctx is not typed"
      | _ -> failwith "it is not a context or it is not eta expanded"
    end
  | Term.Const(_,md,id) ->
    List.assoc id !proof_env
      (*
  | Term.App(Term.Lam(_,id,Some ty, te), a, args) as t->
    let ty' = extract_type ty in
    let poly_vars = extract_poly_vars ty' in
    let td,thm = instr_of_proof ((id,(poly_vars,ty'))::ctx) te in
    let td' = mk_app_term (mk_abs_term (mk_var (mk_name [] (string_of_ident id)) (instr_of_type ty')) td)
        (instr_of_term ctx a) in
    let beta = mk_betaConv td' in
    let sym = mk_sym beta in
    let eqMp = mk_eqMp thm sym in
    td',eqMp *)
    (*
    begin
      match Env.one t with
      | OK None -> assert false
      | OK Some t' ->
        let td,thm = instr_of_proof ctx t' in
        let ty' = extract_type ty in
        let td' =
          mk_app_term (mk_abs_term (mk_var (mk_name [] (string_of_ident id)) (instr_of_type ty')) td)
            (instr_of_term ctx a) in
        let beta = mk_betaConv td' in
        let sym = mk_sym beta in
        let eqMp = mk_eqMp thm sym in
        td',eqMp
      | Err _ -> assert false
    end
    *)
  | _ -> (Pp.print_term Format.std_formatter t; failwith "instr_of_proof")

let mk_prelude lc name = Env.init name (* (Examples.test ()) *)

(* Assume that one defines an external type operator id *)
let define_hol_type id = type_env := id::!type_env


let define_hol_const id te = mk_const (mk_name [] (string_of_ident id)) (instr_of_term [] te)

let define_axiom term =
  let term' = instr_of_term [] term in
  (*
  mk_thm term' (mk_hyp []) (mk_axiom (mk_hyp []) term') *)
  mk_axiom (mk_hyp []) term'

let define_thm term hyp thm =
  mk_thm term hyp thm

let mk_declaration lc id pty : unit =
  begin
    match Env.declare_constant lc id pty with
    | OK _ -> ()
    | Err err -> Errors.fail_env_error err
  end;
  if is_hol_sort pty then
    define_hol_type id
  else
  if is_hol_type pty then
    begin
      (* no implementation provided for that constant, then it is external *)
      let pty' = extract_type pty in
      let poly_vars = extract_poly_vars pty' in
      const_env := (id, (poly_vars,pty'))::!const_env;
    end
  else
  if is_hol_proof pty then
    let term = extract_term pty in
    let ax = define_axiom term in
    let term' = instr_of_term [] term in
    proof_env := (id,(term',ax))::!proof_env
  else
    failwith "case not handle"

let mk_definable lc id pty : unit = failwith "definable symbol without definition... should not happend"

let mk_definition lc id pty_opt pte : unit =
  let ty =
    match pty_opt with
    | Some ty -> ty
    | None -> failwith "missing type in the definition"
  in
  begin
    match Env.define lc id pte pty_opt with
    | OK _ -> ()
    | Err err -> Errors.fail_env_error err
  end;
  if is_hol_type ty then
    begin
      let ty' = extract_type ty in
      let poly_vars = extract_poly_vars ty' in
      const_env := (id, (poly_vars,ty'))::!const_env;
      define_hol_const id pte
    end
  else
  if is_hol_proof ty then
    let _,thm = instr_of_proof [] pte in
    let te = instr_of_term [] (extract_term ty) in
    proof_env :=(id,(te,thm))::!proof_env;
    mk_thm (instr_of_term [] (extract_term ty)) (mk_hyp []) thm
  else
    failwith "case not handle"


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
