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
  let def = [Int(xthm);Def;Pop;Int(xconst);Def;Pop] in
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

let mk_deductAntisym =
  let seen = Hashtbl.create 87 in
  fun thml thmr ->
    let instr = [Load thml;Load thmr;DeductAntisym] in
    update (thml,thmr) instr seen

let mk_absThm =
  let seen = Hashtbl.create 87 in
  fun v thm ->
    let instr = [Load v; Load thm;AbsThm] in
    update (v,thm) instr seen

let mk_eqMp =
  let seen = Hashtbl.create 87 in
  fun thml thmr ->
    let instr = [Load thmr; Load thml;EqMp] in
    update (thml,thmr) instr seen

let mk_assume =
  let seen = Hashtbl.create 87 in
  fun term ->
    let instr = [Load term;Assume] in
    update term instr seen

let mk_proveHyp =
  let seen = Hashtbl.create 87 in
  fun thml thmr ->
    let instr = [Load thmr; Load thml; ProveHyp] in
    update (thml,thmr) instr seen


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

let mk_axiom_forall =
  let seen = Hashtbl.create 87 in
  fun p ty ->
  let env_type = [mk_list [ty; mk_name [] "A"]] in
  let env_var = [] in
  let var = mk_var (mk_name [] "x") (mk_arrow_type ty mk_bool_type) in
  let tl = mk_appTerm (mk_absTerm var (mk_forall_term (mk_varTerm var) ty)) p in
  let tr = mk_appTerm (mk_absTerm var (mk_equal_term (mk_varTerm var) (mk_absTerm (mk_var (mk_name [] "x") ty) mk_true_term) (mk_arrow_type ty mk_bool_type))) p in
  let refl = mk_refl p in
  let term = mk_equal_term (mk_forall_term p ty) (mk_equal_term p (mk_absTerm (mk_var (mk_name [] "v") mk_bool_type) mk_true_term) (mk_arrow_type ty mk_bool_type)) mk_bool_type in
  let instr = beta_equal (mk_appThm (mk_subst mk_axiom_forall_ env_type env_var) refl) tl tr in
  term, S.empty, update (p,ty) [Load instr] seen

let mk_rule_intro_forall =
  let seen = Hashtbl.create 87 in
  fun name ty te thm ->
    let var = mk_var name ty in
    let _,_,true_thm = mk_axiom_true in
    let absThm = mk_absThm var (mk_deductAntisym thm true_thm) in
    let lambda = mk_absTerm var te in
    let _,_,forall_thm = mk_axiom_forall lambda ty in
    let sym = mk_sym forall_thm in
    let eqMp = mk_eqMp absThm sym in
    let term = mk_forall_term lambda ty in
    term, S.empty, (update (name,ty,te,thm) [Load eqMp] seen)

let mk_rule_elim_forall =
  let seen = Hashtbl.create 87 in
  fun thm lambda ty t ->
    let _,_,tforall = mk_axiom_forall lambda ty in
    let eqMp = mk_eqMp thm tforall in
    let refl = mk_refl t in
    let appThm = mk_appThm eqMp refl in
    let te = mk_appTerm (mk_absTerm (mk_var (mk_name [] "v") ty) mk_true_term) t in
    let beta = mk_betaConv te in
    let _,_, true_thm = mk_axiom_true in
    let thm = mk_eqMp true_thm (mk_sym (mk_trans appThm beta)) in
    let term = mk_appTerm lambda t in
    term, S.empty, (update (thm,lambda,ty,t) [Load thm] seen)



let proj =
  let seen = Hashtbl.create 87 in
  fun thm bool left right ->
  let x = mk_var (mk_name [] "x") mk_bool_type in
  let y = mk_var (mk_name [] "y") mk_bool_type in
  let pleft = mk_absTerm x (mk_absTerm y (mk_varTerm x)) in
  let pright = mk_absTerm x (mk_absTerm y (mk_varTerm y)) in
  let side = if bool then pright else pleft in
  let xory = if bool then mk_varTerm y else mk_varTerm x in
  let _,_,tand = mk_axiom_and left right in
  let eqMp = mk_eqMp thm tand in
  let refl = mk_refl side in
  let appThm = mk_appThm eqMp refl in
  let vf = mk_var (mk_name [] "f") mk_binlop_type in
  let f = mk_varTerm vf in
  let tl = mk_appTerm (mk_absTerm vf (mk_appTerm (mk_appTerm f left) right)) side in
  let tr = mk_appTerm (mk_absTerm vf (mk_appTerm (mk_appTerm f mk_true_term) mk_true_term)) side in
  let beta1 = beta_equal appThm tl tr in
  let betaConv = mk_betaConv (mk_appTerm (mk_absTerm x (mk_absTerm y xory)) left) in
  let refl = mk_refl right in
  let appThm = mk_appThm betaConv refl in
  let sym = mk_sym appThm in
  let trans = mk_trans sym beta1 in
  let betaConv = mk_betaConv (mk_appTerm (mk_absTerm x (mk_absTerm y xory)) mk_true_term) in
  let refl = mk_refl mk_true_term in
  let appThm = mk_appThm betaConv refl in
  let trans = mk_trans trans appThm in
  let tl = mk_appTerm (mk_absTerm y (if bool then xory else left)) right in
  let tr = mk_appTerm (mk_absTerm y (if bool then xory else mk_true_term)) mk_true_term in
  let beta = beta_equal trans tl tr in
  let sym = mk_sym beta in
  let _,_,true_thm = mk_axiom_true in
  let instr = mk_eqMp true_thm sym in
  update (thm, bool, left, right) [Load instr] seen

let proj_left =
  let seen = Hashtbl.create 87 in
  fun thm left right ->
    let instr = proj thm false left right in
    update (thm,left,right) [Load instr] seen

let proj_right =
  let seen = Hashtbl.create 87 in
  fun thm left right ->
    let instr = proj thm true left right in
    update (thm,left,right) [Load instr] seen

let mk_rule_intro_impl =
  let seen = Hashtbl.create 87 in
  fun thm p q ->
    let assume = mk_assume p in
    let _,_,thm_true = mk_axiom_true in
    let deduct = mk_deductAntisym assume thm_true in
    let vf = mk_var (mk_name [] "f") mk_binlop_type in
    let f = mk_varTerm vf in
    let refl = mk_refl f in
    let appThm = mk_appThm refl deduct in
    let deduct = mk_deductAntisym thm thm_true in
    let appThm = mk_appThm appThm deduct in
    let absThm = mk_absThm vf appThm in
    let _,_,tand = mk_axiom_and p q in
    let sym = mk_sym tand in
    let eqMp = mk_eqMp absThm sym in
    let assume = mk_assume (mk_and_term p q) in
    let proj_left = proj_left assume p q in
    let deduct = mk_deductAntisym eqMp proj_left in
    let _,_,timpl = mk_axiom_impl p q in
    let sym = mk_sym timpl in
    let eqMp = mk_eqMp deduct sym in
    let term = mk_impl_term p q in
    term, S.empty, update (thm,p,q) [Load eqMp] seen

let mk_rule_elim_impl =
  let seen = Hashtbl.create 87 in
  fun thmp thmimpl p q ->
    let _,_,timpl = mk_axiom_impl p q in
    let assume = mk_assume (mk_impl_term p q) in
    let eqMp = mk_eqMp assume timpl in
    let sym = mk_sym eqMp in
    let assume = mk_assume p in
    let eqMp = mk_eqMp assume sym in
    let proj_right = proj_right eqMp p q in
    let proveHyp = mk_proveHyp proj_right thmp in
    let proveHyp = mk_proveHyp proveHyp thmimpl in
    q, S.empty, update (thmp, thmimpl,p,q) [Load proveHyp] seen


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
