type instr =
  | String of string
  | Int of int
  | AbsTerm
  | AbsThm
  | AppTerm
  | AppThm
  | Assume
  | Axiom
  | BetaConv
  | Cons
  | Const
  | ConstTerm
  | DeductAntisym
  | Def
  | DefineConst
  | DefineConstList
  | DefineTypeOp
  | EqMp
  | HdTl
  | Load of int
  | Nil
  | OpType
  | Pop
  | Pragma
  | ProveHyp
  | Ref
  | Refl
  | Remove
  | Subst
  | Sym
  | Thm
  | Trans
  | TypeOp
  | Var
  | VarTerm
  | VarType
  | Version

module S = Set.Make (struct type t = int let compare = compare end)

type name = int

type var = int

type term = int

type tyop = int

type ty = int

type const = int * int

type hyp = S.t

type thm = int

type cmd = unit



type dict = (int, instr list) Hashtbl.t

type generated_entry = S.t

let dict : dict = Hashtbl.create 87

let const_seen  = Hashtbl.create 87

let generated_entry : generated_entry ref =  ref S.empty

let counter : int ref = ref 0

let version = [Int(6);Version]

let cmds : instr list ref = ref version

let save instrs =
  cmds := !cmds@instrs

let mk_ref n = [Int(n);Ref]

let update entry instrs seen =
  if Hashtbl.mem seen entry then
    Hashtbl.find seen entry
  else
    let x = !counter in
    incr counter;
    Hashtbl.add seen entry x;
    let def = [Int(x);Def] in
    Hashtbl.add dict x (instrs@def);
    x

let update_cons name instrs =
  let xthm = !counter in
  incr counter;
  let xconst = !counter in
  incr counter;
  let def = [Int(xthm);Def;Int(xconst);Def;Pop;Pop] in
  save (instrs@def);
  generated_entry := S.add xthm (S.add xconst !generated_entry);
  Hashtbl.add const_seen name (xconst, xthm)

let mk_name =
  let rec mk_namespace l =
    List.fold_left (fun s x -> s^x^".") "" l
  in
  let seen = Hashtbl.create 87 in
  fun namespace str ->
    let name = (mk_namespace namespace)^str in
    update name [String name] seen

let arrow_name = mk_name [] "->"

let bool_name = mk_name [] "bool"

let equal_name = mk_name [] "="

let true_name = mk_name [] "T"

let and_name = mk_name [] "/\\\\"

let impl_name = mk_name [] "==>"

let forall_name = mk_name [] "!"

let mk_var =
  let seen = Hashtbl.create 87 in
  fun name ty ->
    let instr = [Load name;Load ty;Var] in
    update (name,ty) instr seen


let mk_list =
  let seen = Hashtbl.create 87 in
  fun l ->
    let instr =  List.fold_left (fun l x -> (Load x)::(l@[Cons])) [Nil] l in
    update l instr seen


let mk_tyop =
  let seen = Hashtbl.create 87 in
  fun name ->
    let instr = [Load name; TypeOp] in
    update name instr seen


let mk_varType =
  let seen = Hashtbl.create 87 in
  fun name ->
    update name [Load name;VarType] seen

let ty_of_tyop =
  let seen = Hashtbl.create 87 in
  fun tyop l ->
    let l' = mk_list l in
    let instr = [Load tyop;Load l';OpType] in
    update (tyop,l') instr seen

let mk_arrow_type =
  let seen = Hashtbl.create 87 in
  fun tyl tyr ->
    let instr = [Load(ty_of_tyop (mk_tyop arrow_name) [tyr;tyl])] in
    update (tyl,tyr) instr seen

let mk_bool_type =
  ty_of_tyop (mk_tyop bool_name) []

let mk_equal_type =
  let seen = Hashtbl.create 87 in
  fun ty ->
    let instr = [Load(mk_arrow_type ty (mk_arrow_type ty (mk_bool_type)))] in
    update ty instr seen

let mk_true_type = mk_bool_type

let mk_binlop_type =  mk_arrow_type mk_bool_type (mk_arrow_type mk_bool_type mk_bool_type)

let mk_and_type = mk_binlop_type

let mk_impl_type = mk_binlop_type

let mk_forall_type =
  let seen = Hashtbl.create 87 in
  fun ty ->
    let instr = [Load (mk_arrow_type (mk_arrow_type ty mk_bool_type) mk_bool_type)] in
    update ty instr seen


let mk_appTerm =
  let seen = Hashtbl.create 87 in
  fun f t ->
    let instr = [Load f;Load t;AppTerm] in
    update (f,t) instr seen

let mk_absTerm =
  let seen = Hashtbl.create 87 in
  fun v t ->
    let instr = [Load v;Load t;AbsTerm] in
    update (v,t) instr seen

let mk_varTerm =
  let seen = Hashtbl.create 87 in
  fun var ->
    let instr = [Load var;VarTerm] in
    update var instr seen

let term_of_const =
  let seen = Hashtbl.create 87 in
  fun const ty ->
    let instr = [Load (fst const);Load ty;ConstTerm] in
    update (const,ty) instr seen

let const_of_name =
  let seen = Hashtbl.create 87 in
  fun name ->
    if Hashtbl.mem const_seen name then
      Hashtbl.find const_seen name
    else
      let instr = [Load name;Const] in
      (update name instr seen, -1)

let mk_equal_term =
  let seen = Hashtbl.create 87 in
  fun l r ty ->
    let ty' = mk_equal_type ty in
    let cst = term_of_const (const_of_name equal_name) ty' in
    let instr = [Load(mk_appTerm (mk_appTerm cst l) r)] in
    update (l,r,ty) instr seen

let mk_true_term = term_of_const (const_of_name true_name) mk_true_type

let mk_and_term =
  let seen = Hashtbl.create 87 in
  fun l r ->
    let cst = term_of_const (const_of_name and_name) mk_and_type in
    let instr = [Load(mk_appTerm (mk_appTerm cst l) r)] in
    update (l,r) instr seen

let mk_impl_term =
  let seen = Hashtbl.create 87 in
  fun l r ->
    let cst = term_of_const (const_of_name impl_name) mk_impl_type in
    let instr = [Load(mk_appTerm (mk_appTerm cst l) r)] in
    update (l,r) instr seen

let mk_forall_term =
  let seen = Hashtbl.create 87 in
  fun p ty ->
    let cst = term_of_const (const_of_name forall_name) (mk_forall_type ty) in
    let instr = [Load(mk_appTerm cst p)] in
    update (p,ty) instr seen

let rec mk_hyp l = List.fold_left (fun set x -> S.add x set) S.empty l

let list_of_hyp hyp =
  S.fold (fun x l -> x::l) hyp []

let mk_axiom =
  let seen = Hashtbl.create 87 in
  fun hyp term ->
    let hyp = mk_list (list_of_hyp hyp) in
    let instr = [Load hyp;Load term;Axiom] in
    update (hyp,term) instr seen

let mk_axiom_true = mk_true_term, S.empty, mk_axiom S.empty mk_true_term

let mk_axiom_and_ =
  let vx = mk_var (mk_name [] "x") mk_bool_type in
  let vy = mk_var (mk_name [] "y") mk_bool_type in
  let x = mk_varTerm vx in
  let y = mk_varTerm vy in
  let tand = mk_absTerm vx (mk_absTerm vy (mk_and_term x y)) in
  let vf = mk_var (mk_name [] "f") mk_binlop_type in
  let f = mk_varTerm vf in
  let rl = mk_absTerm vf (mk_appTerm (mk_appTerm f x) y) in
  let rr = mk_absTerm vf (mk_appTerm (mk_appTerm f mk_true_term) mk_true_term) in
  let rhs = mk_absTerm vx (mk_absTerm vy (mk_equal_term rl rr
                                            (mk_arrow_type mk_binlop_type mk_bool_type))) in
  let term = mk_equal_term tand rhs mk_binlop_type in
  mk_axiom S.empty term



let thm_of_const const =
  snd const

let mk_refl =
  let seen = Hashtbl.create 87 in
  fun term ->
    update term [Load term;Refl] seen

let mk_appThm =
  let seen = Hashtbl.create 87 in
  fun thml thmr ->
    update (thml,thmr) [Load thml;Load thmr;AppThm] seen

let mk_sym =
  let seen = Hashtbl.create 87 in
  fun thm ->
    update thm [Load thm;Sym] seen

let mk_betaConv =
  let seen = Hashtbl.create 87 in
  fun term ->
    update term [Load term;BetaConv] seen

let mk_trans =
  let seen = Hashtbl.create 87 in
  fun thml thmr ->
    let instr = [Load thml;Load thmr;Trans] in
    update (thml,thmr) instr seen

let mk_subst =
  let seen = Hashtbl.create 87 in
  fun thm env_type env_var ->
    let env_type' = mk_list env_type in
    let env_var' = mk_list env_var in
    let subst = mk_list [ env_var'; env_type'] in
    let instr = [Load subst; Load thm; Subst] in
    update (thm,env_type,env_var) instr seen

let mk_const name term =
  let instrs = [Load name; Load term;DefineConst] in
  update_cons name instrs

let mk_thm term hyp thm =
  save [Load thm;Load (mk_list (list_of_hyp hyp));Load term;Thm]

let mk_remove x = [Int(x);Remove;Pop]

let beta_equal thm tl tr =
  let betal = mk_sym (mk_betaConv tl) in
  let betar = mk_betaConv tr in
  let trans = mk_trans betal thm in
  mk_trans trans betar

let mk_axiom_and =
  let seen = Hashtbl.create 87 in
  fun l r ->
  let vx = mk_var (mk_name [] "x") mk_bool_type in
  let vy = mk_var (mk_name [] "y") mk_bool_type in
  let x = mk_varTerm vx in
  let y = mk_varTerm vy in
  let t1 = (mk_absTerm vy (mk_and_term x y)) in
  let fv = mk_var (mk_name [] "f") (mk_binlop_type) in
  let ft = mk_varTerm fv in
  let rl = mk_absTerm fv (mk_appTerm (mk_appTerm ft x) y) in
  let rr = mk_absTerm fv (mk_appTerm (mk_appTerm ft mk_true_term) mk_true_term) in
  let t2 = mk_absTerm vy (mk_equal_term rl rr (mk_arrow_type mk_binlop_type mk_bool_type)) in
  let refl = mk_refl l in
  let tl = mk_appTerm (mk_absTerm vx t1) l in
  let tr = mk_appTerm (mk_absTerm vx t2) l in
  let beta1 = beta_equal (mk_appThm mk_axiom_and_ refl) tl tr in
  let rl = mk_absTerm fv (mk_appTerm (mk_appTerm ft l) y) in
  let t2 = mk_equal_term rl rr (mk_arrow_type mk_binlop_type mk_bool_type) in
  let refl = mk_refl r in
  let tl = mk_appTerm (mk_absTerm vy (mk_and_term l y)) r in
  let tr = mk_appTerm (mk_absTerm vy t2) r in
  let instr = [Load (beta_equal (mk_appThm beta1 refl) tl tr)] in
  let term = mk_equal_term (mk_and_term l r) (mk_equal_term (mk_absTerm fv (mk_appTerm (mk_appTerm ft l) r)) (mk_absTerm fv (mk_appTerm (mk_appTerm ft mk_true_term) mk_true_term)) (mk_arrow_type mk_binlop_type mk_bool_type)) mk_bool_type in
  term, S.empty, update (l,r) instr seen

let mk_axiom_impl_ =
  let vx = mk_var (mk_name [] "x") mk_bool_type in
  let vy = mk_var (mk_name [] "y") mk_bool_type in
  let x = mk_varTerm vx in
  let y = mk_varTerm vy in
  let implt = mk_absTerm vx (mk_absTerm vy (mk_impl_term x y)) in
  let r = mk_equal_term (mk_and_term x y) x mk_bool_type in
  let term = mk_equal_term implt (mk_absTerm vx (mk_absTerm vy r)) mk_binlop_type in
  mk_axiom S.empty term

let mk_axiom_impl =
  let seen = Hashtbl.create 87 in
  fun l r ->
  let vx = mk_var (mk_name [] "x") mk_bool_type in
  let vy = mk_var (mk_name [] "y") mk_bool_type in
  let x = mk_varTerm vx in
  let y = mk_varTerm vy in
  let tl = mk_appTerm (mk_absTerm vx (mk_absTerm vy (mk_impl_term x y))) l in
  let tr = mk_appTerm (mk_absTerm vx (mk_absTerm vy
                                        (mk_equal_term (mk_and_term x y) x mk_bool_type))) l in
  let refl = mk_refl l in
  let beta1 = beta_equal (mk_appThm mk_axiom_impl_ refl) tl tr in
  let tl = mk_appTerm (mk_absTerm vy (mk_impl_term l y)) r in
  let tr = mk_appTerm (mk_absTerm vy (mk_equal_term (mk_and_term l y) l mk_bool_type)) r in
  let refl = mk_refl r in
  let instr = beta_equal (mk_appThm beta1 refl) tl tr in
  let term = mk_equal_term (mk_impl_term l r) (mk_equal_term (mk_and_term l r) l mk_bool_type)
      mk_bool_type in
  term, S.empty, update (l,r) [Load instr] seen

let mk_axiom_forall_ =
  let ty = mk_varType (mk_name [] "A") in
  let vx = mk_var (mk_name [] "x") (mk_arrow_type ty mk_bool_type) in
  let x = mk_varTerm vx in
  let tforall = mk_absTerm vx (mk_forall_term x ty) in
  let r = mk_absTerm (mk_var (mk_name [] "v") ty) mk_true_term in
  let instr = mk_equal_term tforall (mk_absTerm vx (mk_equal_term x r (mk_arrow_type ty mk_bool_type))) (mk_arrow_type (mk_arrow_type ty mk_bool_type) mk_bool_type) in
  mk_axiom S.empty instr

let mk_axiom_forall p ty =
  let env_type = [mk_list [ty; mk_name [] "A"]] in
  let env_var = [] in
  let var = mk_var (mk_name [] "x") (mk_arrow_type ty mk_bool_type) in
  let tl = mk_appTerm (mk_absTerm var (mk_forall_term (mk_varTerm var) ty)) p in
  let tr = mk_appTerm (mk_absTerm var (mk_equal_term (mk_varTerm var) (mk_absTerm (mk_var (mk_name [] "x") ty) mk_true_term) (mk_arrow_type ty mk_bool_type))) p in
  let refl = mk_refl p in
  let term = mk_equal_term (mk_forall_term p ty) (mk_equal_term p (mk_absTerm (mk_var (mk_name [] "v") mk_bool_type) mk_true_term) (mk_arrow_type ty mk_bool_type)) mk_bool_type in
  term, S.empty, beta_equal (mk_appThm (mk_subst mk_axiom_forall_ env_type env_var) refl) tl tr


let rec load_instr n =
  if S.mem n !generated_entry then
    string_of_instrs (mk_ref n)
  else
    let instr = string_of_instrs (Hashtbl.find dict n) in
    generated_entry := S.add n !generated_entry;
    instr

and string_of_instr x =
  match x with
  | String(s) -> Printf.sprintf "\"%s\"\n" s
  | Int(i) -> (string_of_int i)^"\n"
  | AbsTerm -> "absTerm\n"
  | AbsThm -> "absThm\n"
  | AppTerm -> "appTerm\n"
  | AppThm -> "appThm\n"
  | Assume -> "assume\n"
  | Axiom -> "axiom\n"
  | BetaConv -> "betaConv\n"
  | Cons -> "cons\n"
  | Const -> "const\n"
  | ConstTerm -> "constTerm\n"
  | DeductAntisym -> "deductAntisym\n"
  | Def -> "def\n"
  | DefineConst -> "defineConst\n"
  | DefineConstList -> "defineConstList\n"
  | DefineTypeOp -> "defineTypeOp\n"
  | EqMp -> "eqMp\n"
  | HdTl -> "hdTl\n"
  | Load(n) -> load_instr n
  | Nil -> "nil\n";
  | OpType -> "opType\n";
  | Pop -> "pop\n";
  | Pragma -> "pragma\n";
  | ProveHyp -> "proveHyp\n";
  | Ref -> "ref\n";
  | Refl -> "refl\n";
  | Remove -> "remove\n";
  | Subst -> "subst\n";
  | Sym -> "sym\n";
  | Thm -> "thm\n";
  | Trans -> "trans\n";
  | TypeOp -> "typeOp\n"
  | Var -> "var\n"
  | VarTerm -> "varTerm\n"
  | VarType -> "varType\n"
  | Version -> "version\n"

and string_of_instrs instrs = List.fold_left (fun s x -> s^(string_of_instr x)) "" instrs

let remove_keys () =
  let rec remove x =
    if x = 0 then []
    else
      if S.mem (x-1) !generated_entry then
        mk_remove (x-1)@(remove (x-1))
      else
        remove (x-1)
  in
  remove !counter

let debug () = save [String("debug");Pragma]

(* need to generate the string before removing_keys so that load instruction are executed *)
let to_string () =
  let str = string_of_instrs !cmds in
  str^(string_of_instrs (remove_keys ()))
