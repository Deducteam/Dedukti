open Basic

type ty_ctx = ident list

let hol_module = hstring "hol"
let hol_type = hstring "type"
let hol_eta = hstring "eta"
let hol_arrow = hstring "arrow"
let hol_forall = hstring "forall"
let hol_leibniz = hstring "leibniz"
let hol_impl = hstring "impl"
let hol_prop = hstring "prop"
let hol_eps = hstring "eps"
let hol_forall_kind_type = hstring "forall_kind_type"
let hol_forall_kind_prop = hstring "forall_kind_prop"

let logic_module = hstring "logic"

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

let list_of_hol_theorem_with_eq =
  [hstring "eq_minus_O"; hstring "eq_minus_S_pred"; hstring "eq_to_eqb_true"; hstring "eq_to_eqb_false"; hstring "eq_times_div_minus_mod"; hstring "eq_to_bijn"; hstring "eq_minus_gcd_aux"; hstring "eq_div_O"; hstring "eq_minus_gcd"; hstring "eq_times_plus_to_congruent"; hstring "eq_mod_to_divides"; hstring "eq_fact_pi_p"]

let is_delta_rw cst =
  match cst with
  | Term.Const(_,md,id) ->
    not (md === logic_module) && Str.(string_match (regexp "eq_\\|sym_eq") (string_of_ident id) 0) && not (List.exists (fun thm -> id === thm) list_of_hol_theorem_with_eq)
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

let rec arguments_needed rw =
  let ty =
    match rw with
    | Term.Const(lc,md,id) ->
      begin
        match Env.get_type lc md id with
        | OK ty -> ty
        | Err err -> Errors.fail_signature_error err
      end
    | _ -> assert false
  in
  let rec aux t n =
    let is_forall_prop md id =
      md === hol_module && id === hol_forall_kind_prop
    in
    let is_forall md id =
      md === hol_module && id === hol_forall
    in
    match t with
    | Term.App(Term.Const(_,md,id),_,_) when md === hol_module && id === hol_leibniz -> n
    | Term.App(Term.Const(_,md,id),Term.Lam(_,_,_,te),[]) when is_forall_prop md id -> aux te (n+1)
    | Term.App(Term.Const(_,md,id),_,[Term.Lam(_,_,_,te)]) when is_forall md id -> aux te (n+1)
    | Term.App(Term.Const(_,md,id), t', []) when md === hol_module && id === hol_eps -> aux t' n
    | _ -> Format.eprintf "debug: %a@." Pp.print_term rw; assert false
  in
  aux ty 0

let rec compile_term ty_ctx te_ctx te =
  match te with (*
  | Term.App(rw, a, args) when is_delta_rw rw && List.length (a::args) > arguments_needed rw ->
    let rec find_ctx args n =
      match args,n with
      | [],_ -> assert false
      | [_],_ -> assert false
      | ctx::r::t,0 -> [], ctx,r,t
      | x::args',_ ->
        let l,ctx,r,t = find_ctx args' (n-1) in
        x::l,ctx,r,t
    in
    let l,ctx,r,t = find_ctx (a::args) (arguments_needed rw) in
    let ty,l =
      match l with
      | [] -> assert false
      | x::t -> x,t
    in
    let te = Term.mk_App rw ty l in
    let red = {Reduction.beta = true; Reduction.select = Some (fun x ->
        match x with
        | Rule.Delta(md,id) when md = hstring "hol" && id = hstring "leibniz" -> false
        | _ -> true) } in
    let te' =
      match Env.reduction ~red:red Reduction.Whnf te with
      | OK t -> t
      | Err err -> Errors.fail_env_error err
    in
    let ty' =
      match Env.infer te' with
      | OK ty ->
        begin
          match Env.reduction ~red:red Reduction.Hnf ty with
          | OK t -> t
          | Err err -> Errors.fail_env_error err
        end
      | Err err -> Errors.fail_env_error err
    in
    let x, y =
    match ty' with
      | Term.App(_,Term.App(cst, ty, [tel;ter]), []) when is_hol_const hol_leibniz cst -> tel, ter
      | _ -> Format.eprintf "debug: %a@." Pp.print_term ty'; assert false
    in
    let ty' = compile_term ty_ctx te_ctx ty in
    let x' = compile_term ty_ctx te_ctx x in
    let y' = compile_term ty_ctx te_ctx y in
    let p' = compile_term ty_ctx te_ctx ctx in
    let px' = compile_term ty_ctx te_ctx r in
    let rw' = compile_term ty_ctx te_ctx rw in
    let stuff =
      Ast.App(
        Ast.App(
          Ast.App(
            Ast.App(
              Ast.App(
                Ast.App(
                  Ast.Const(hstring "", hstring "eq_ind"),
                  ty'),
                x'),
              p'),
            px'),
          y'),
        rw')
    in
    List.fold_left (fun t a -> Ast.App(t,compile_term ty_ctx te_ctx a)) stuff t *)
  | Term.App(c, Term.Lam(_, var, _, ty), []) when is_hol_const hol_forall_kind_type c ->
    let ty' = compile_term (var::ty_ctx) te_ctx ty in
    Ast.Forall(var, Ast.Type, ty')
  | Term.Const(_,md,id) when is_hol_const hol_prop te -> Ast.Prop
  | Term.App(c,left,[right]) when is_hol_const hol_arrow c ->
    let left' = compile_term ty_ctx te_ctx left in
    let right' = compile_term ty_ctx te_ctx right in
    Ast.Impl(left',right')
  | Term.App(cst, Term.Lam(_,x, Some ty, te'), []) when is_hol_const hol_forall_kind_prop cst ->
    assert (is_hol_const hol_type ty);
    Ast.Forall(x, Ast.Type, compile_term (x::ty_ctx) (te_ctx) te')
  | Term.App(cst, ty, [Term.Lam(_,id, Some tyvar, te)]) when is_hol_const hol_forall cst ->
    let ty' = compile_term ty_ctx te_ctx ty in
    let te' = compile_term ty_ctx ((id, ty')::te_ctx) te in
    Ast.Forall(id, ty', te')
  | Term.App(cst, tel, [ter]) when is_hol_const hol_impl cst ->
    let tel' = compile_term ty_ctx te_ctx tel in
    let ter' = compile_term ty_ctx te_ctx ter in
    Ast.Impl(tel',ter')
      (*
  | Term.App(cst, ty, [tel;ter]) when is_hol_const hol_leibniz cst ->
    let ty' = compile_term ty_ctx te_ctx ty in
    let tel' = compile_term ty_ctx te_ctx tel in
    let ter' = compile_term ty_ctx te_ctx ter in
    Ast.Eq(ty',(tel',ter')) *)
  | Term.Const(lc,md,id) ->
    Ast.Const(md,id)
  | Term.DB(_,var,n) ->
    Ast.Var(var)
  | Term.Lam(_,id, Some cst, te) when is_hol_const hol_type cst ->
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
  | Term.App(cst, a, []) when is_hol_const hol_eta cst || is_hol_const hol_eps cst ->
    compile_term ty_ctx te_ctx a
  | _ -> assert false


let compile_declaration md id ty =
      match ty with
    | Term.App(cst,a,[]) when is_hol_const hol_eta cst ->
      Ast.Parameter((md,id), compile_term [] [] a)
    | Term.App(cst,a,[]) when is_hol_const hol_eps cst ->
      Ast.Axiom((md,id), compile_term [] [] a)
    | Term.Const(_,_,_) when is_hol_const hol_type ty ->
      Ast.Parameter((md,id), Ast.Type)
    | _ -> assert false

let compile_definition md id ty term =
  match ty with
  | Term.App(cst,a,[]) when is_hol_const hol_eta cst ->
    Ast.Constant((md,id), compile_term [] [] a, compile_term [] [] term)
  | Term.App(cst,a,[]) when is_hol_const hol_eps cst ->
    Ast.Theorem((md,id), compile_term [] [] a, compile_term [] [] term)
  | _ -> assert false
