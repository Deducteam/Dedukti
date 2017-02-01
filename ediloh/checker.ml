open Basic

(*

  (load_typeop arrow_str)@
  tyl@tyr@
  [Nil;Cons;Cons;OpType]
*)

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

type ast = instr list

let string_of_instr x =
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

let mk_name str = [String str]

let mk_appTerm f t =
  f@t@[AppTerm]

let mk_var str ty =
  (mk_name str)@ty@[Var]

let mk_absTerm v t =
  v@t@[AbsTerm]

let mk_varTerm str ty =
  [String(str)]@ty@[Var;VarTerm]

let mk_term_of_var var = var@[VarTerm]

let mk_termconst str ty=
  [String str; Const]@ty@[ConstTerm]

let mk_const str ty l =
  let const = mk_termconst str ty in
  let rec apply term l =
    match l with
    | [] -> term
    | x::t -> apply (mk_appTerm term x) t
  in
  apply const l

let mk_varType str =
  [String(str);VarType]


let rec to_list l =
  match l with
  | [] -> [Nil]
  | x::t -> x@(to_list t)@[Cons]

let list_of_pair (x,y) =
  x@y@[Nil;Cons;Cons]

module S = Set.Make(struct type t = ast let compare = compare end)

(* TODO : as it is, hypothesis are not properly handled. Indeed we cannot guarantee that two terms are always constructed by the exactly the same instructions. One way to solve this issue is to use the dictionnary proposed by Open Theory. Though for the moment, I don't know how to properly handle this dictionnary. Furthermore, we shouldn't care about hyp. *)
type thm =
  { proof : ast;
    hyp : S.t;
    term : ast
  }

let mk_hyp hyp = to_list (S.fold (fun x l -> x::l) hyp [])

let mk_axiom term hyp =
  let proof = (mk_hyp hyp)@term@[Axiom] in
  {proof = proof ; hyp = hyp ; term = term}

let string_of_ast ast = List.fold_left (fun s x -> (string_of_instr x)^s) "" ast

let namespace = ref ""

let (cmds:ast ref) = ref []

let add_instr cmd =
  cmds := cmd::!cmds

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

let (===) = Basic.ident_eq

let is_hol_const c t =
  match t with
  | Term.Const(_, m, id) -> (m === hol_module) &&  (c === id)
  | _ -> false

let is_hol_sort t = is_hol_const hol_sort t

let is_hol_eta t = is_hol_const hol_eta t

let is_hol_arrow t = is_hol_const hol_arrow t

let is_hol_forall t = is_hol_const hol_forall t

let is_hol_impl t = is_hol_const hol_impl t

let is_hol_prop t = is_hol_const hol_prop t

let is_hol_eps t = is_hol_const hol_eps t

let is_hol_forall_kind_type t = is_hol_const hol_forall_kind_type t

let is_hol_type t =
  match t with
  | Term.App (c, _, _) -> is_hol_eta c
  | _ -> false

let is_hol_proof t =
  match t with
  | Term.App (c, _, _) -> is_hol_eps c
  | _ -> false

(* ************************************************* *)


let load_typeop str =
  [String str; TypeOp]

let load_const str=
  [String str; Const]

let forall_str = "!"

let equal_str = "="

let true_str = "T"

let arrow_str = "->"

let bool_str = "bool"

let impl_str = "==>"

let and_str = "/\\\\"

let or_str = "\\\\/"

let debug =
  [String "debug"; Pragma]

let type_arrow tyl tyr =
  (load_typeop arrow_str)@
  tyl@tyr@
  [Nil;Cons;Cons;OpType]

let type_bool =
  (load_typeop bool_str)@
  [Nil;OpType]

let type_binlop =
  type_arrow type_bool (type_arrow type_bool type_bool)

let type_equal ty =
  type_arrow ty (type_arrow ty type_bool)

let type_true = type_bool

let type_and = type_binlop

let type_impl = type_binlop

let type_forall ty = type_arrow (type_arrow ty type_bool) type_bool

let term_equal l r ty =
  mk_const equal_str (type_equal ty) [l;r]

let term_true =
  mk_const true_str type_true []

let term_and l r =
  mk_const and_str type_and [l;r]

let term_impl l r =
  mk_const impl_str type_impl [l;r]

let term_forall ty p =
  mk_const forall_str (type_forall ty) [p]

let mk_refl term ty =
  let proof = term@[Refl] in
  let term = term_equal term term ty in
  {proof = proof ; hyp = S.empty ; term = term}

let mk_deductAntisym thml thmr =
  {
    proof = thml.proof@thmr.proof@[DeductAntisym];
    hyp = S.union (S.remove thmr.term thml.hyp) (S.remove thml.term thmr.hyp);
    term = term_equal thml.term thmr.term type_bool
  }

let mk_absThm v thm =
  {
    proof = v@thm.proof@[AbsThm];
    hyp = thm.hyp;
    term = mk_absTerm v thm.term
  }

(* FIXME : return a fake term cause for now i'm unable to handle substituion. The term of the proof should never be used *)
let mk_subst thm env_type env_var =
  let env_type' = to_list (List.map list_of_pair env_type) in
  let env_var' = to_list (List.map list_of_pair env_var) in
  let subst = list_of_pair (env_type',env_var') in
  {proof = subst@thm.proof@[Subst];
   hyp = thm.hyp;
   term = []
  }

let mk_sym thm = {thm with proof = thm.proof@[Sym]}

(* FIXME : return a fake term cause for now i'm unable to handle this. The term of the proof should never be used *)
let mk_eqMp thml thmr =
  {
    proof = thmr.proof@thml.proof@[EqMp];
    hyp = S.union thml.hyp thmr.hyp;
    term = []
  }

let mk_appThm thml thmr =
  {
    proof = thml.proof@thmr.proof@[AppThm];
    hyp = S.union thml.hyp thmr.hyp;
    term = []
  }

let mk_betaConv var t u =
  let term = mk_appTerm (mk_absTerm var t) u in
  {
    proof = term@[BetaConv];
    hyp = S.empty;
    term = []
  }

let mk_trans thml thmr =
  {
    proof = thml.proof@thmr.proof@[Trans];
    hyp = S.union thml.hyp thmr.hyp;
    term = []
  }


let mk_thm thm =
  thm.proof@(mk_hyp thm.hyp)@thm.term@[Thm]

let axiom_true =
  mk_axiom term_true S.empty

let axiom_and =
  let vx = mk_var "x" type_bool in
  let vy = mk_var "y" type_bool in
  let x = mk_term_of_var vx in
  let y = mk_term_of_var vy in
  let andt = mk_absTerm vx (mk_absTerm vy (term_and x y)) in
  let fv = mk_var "f" (type_binlop) in
  let ft = mk_term_of_var fv in
  let rl = mk_absTerm fv (mk_appTerm (mk_appTerm ft x) y) in
  let rr = mk_absTerm fv (mk_appTerm (mk_appTerm ft term_true) term_true) in
  let rhs = mk_absTerm vx (mk_absTerm vy (term_equal rl rr (type_arrow type_binlop type_bool))) in
  let term = term_equal andt rhs type_binlop in
  mk_axiom term S.empty

let instantiate_and_axiom l r = failwith "todo"

let axiom_impl =
  let vx = mk_var "x" type_bool in
  let vy = mk_var "y" type_bool in
  let x = mk_term_of_var vx in
  let y = mk_term_of_var vy in
  let implt = mk_absTerm vx (mk_absTerm vy (term_impl x y)) in
  let r = term_equal (term_and x y) x type_bool in
  let term = term_equal implt (mk_absTerm vx (mk_absTerm vy r)) type_binlop in
  mk_axiom term S.empty

let instantiate_impl_axiom l r = failwith "todo"

(* str_type should only be instantiated with the Subst command. This is to ensure that there is only one axiom forall *)
let axiom_forall =
  let str_term = "x" in
  let str_var = "v" in
  let str_type = "A" in
  let varTy = mk_varType str_type in
  let vx = mk_var str_term (type_arrow varTy type_bool) in
  let tx = mk_term_of_var vx in
  let forallt = (mk_absTerm vx (term_forall varTy tx)) in
  let var = mk_var str_var varTy in
  let r = mk_absTerm var term_true in
  let term = term_equal forallt (mk_absTerm vx (term_equal tx r (type_arrow varTy type_bool))) (type_arrow (type_arrow varTy type_bool) type_bool) in
  mk_axiom term S.empty

let beta_equal thm t1 t2 u1 u2 var =
  let beta1 = mk_sym (mk_betaConv var t1 u2) in
  let beta2 = mk_betaConv var t2 u2 in
  let trans = mk_trans beta1 thm in
  mk_trans trans beta2

(* from \x:(A->bool) . !x = (x = \v . true) produce the theorem
         !lambda:(ty -> bool) = (lambda = \v. true) *)
(* TODO : some code duplication with axiom_forall... *)
let instantiate_forall_axiom ty lambda =
  let var_ty = mk_name "A" in
  let env_type = [var_ty, ty] in
  let var = mk_var "x" (type_arrow ty type_bool) in
  let t1 = (term_forall ty (mk_term_of_var var)) in
  let t2 = term_equal (mk_term_of_var var) (mk_absTerm (mk_var "v" ty) term_true) (type_arrow ty type_bool) in
  let u1 = lambda in
  let u2 = lambda in
  let refl = mk_refl lambda (type_arrow ty type_bool) in
  beta_equal (mk_appThm (mk_subst axiom_forall env_type []) refl) t1 t2 u1 u2 var

let forall_intro var_str ty thm =
  let v = mk_var var_str ty in
  let thml = mk_absThm v (mk_deductAntisym thm axiom_true) in
  let lambda = mk_absTerm v thm.term in
  let thmr = mk_sym (instantiate_forall_axiom ty lambda) in
  let thm = mk_eqMp thml thmr in
  let term = mk_const forall_str (type_arrow (type_arrow ty type_bool) type_bool) [lambda] in
  {proof = thm.proof;
   hyp = thm.hyp;
   term = term
  }

let forall_elim thm lambda ty t =
  let str_var = "v" in
  let instance_forall = instantiate_forall_axiom ty lambda in
  let eqMp = mk_eqMp thm instance_forall in
  let refl = mk_refl t ty in
  let appThm = mk_appThm eqMp refl in
  let beta = mk_betaConv (mk_var str_var ty) term_true t in
  let thm = mk_eqMp axiom_true (mk_sym (mk_trans appThm beta)) in
  let term = mk_appTerm lambda t in
  {
    proof = thm.proof;
    hyp = thm.hyp;
    term = term
  }

let left = mk_absTerm (mk_var "x" type_bool) (mk_absTerm (mk_var "y" type_bool) (mk_varTerm "x" type_bool))
let right = mk_absTerm (mk_var "x" type_bool) (mk_absTerm (mk_var "y" type_bool) (mk_varTerm "y" type_bool))
let proj thm side left right = failwith "todo" (*
  let x = mk_varTerm "x" type_bool in
  let y = mk_varTerm "y" type_bool in
  let ax_and = axiom_and *)



let instr_of_proof ctx t = failwith "todo instr_of_proof"


let extract_proof t =
  match t with
  | Term.App(c, ty, _) when is_hol_eps c -> ty
  | _ -> assert false
(*
let intro_rule ctx t =
  match t with
  | Term.Lam(_,x,Some ty, t') when is_hol_proof ty ->
    let ty' = extract_proof ty in
    (* ASSUMPTION : no conflict with variables names *)
    let q = instr_of_proof ((x,ty')::ctx) t' in
    failwith "intro rule"
  | _ -> failwith "intro_rule is not call on a lambda"
*)


let extract_type t =
  match t with
  | Term.App(c, ty, _) when is_hol_eta c -> ty
  | _ -> assert false


let rec instr_of_type t =
  match t with
  | Term.Const(_,m,id) when is_hol_prop t ->
    type_bool
  | Term.Const(_,m, id) ->
    [String (string_of_ident id); VarType]
  (* ASSUMPTION : no clash in names and id should start with a majuscule *)
  | Term.DB(_,id,i) ->
    [String (string_of_ident id); VarType]
  | Term.App(c, tyl, [tyr]) when is_hol_arrow c ->
    type_arrow (instr_of_type tyl) (instr_of_type tyr)
  | Term.App(c, Term.Lam(_,x, Some tx, ty), []) when is_hol_forall_kind_type c ->
    instr_of_type ty
  | _ -> Pp.print_term Format.std_formatter t; failwith "todo type"


type env = (Basic.ident * Term.term) list

(* TODO add a dictionnary *)
let rec instr_of_term env t =
  match t with
  | Term.App(c, ty, [te])  when is_hol_forall c ->
    (load_const forall_str)@
    (type_forall (instr_of_type ty))@
    [ConstTerm]@
    (instr_of_term env te)@
    [AppTerm]
  | Term.App(c, tel, [ter]) when is_hol_impl c ->
    (load_const impl_str)@
    (type_impl)@
    [ConstTerm]@
    (instr_of_term env tel)@
    [AppTerm]@
    (instr_of_term env ter)@
    [AppTerm]
  | Term.App(f, a, args) ->
    List.fold_left
      (fun instr a -> instr@(instr_of_term env a)@[AppTerm]) (instr_of_term env f) (a::args)
  | Term.Lam(_,x,Some tx, t') when is_hol_sort tx ->
    instr_of_term env t'
  | Term.Lam(_,x,Some tx, t') ->
    let tx' = extract_type tx in
    let it = (instr_of_term ((x,tx')::env) t') in
    let itx = instr_of_type tx' in
    let ix  = (String (string_of_ident x)) in
    [ix]@itx@[Var]@it@[AbsTerm]
  | Term.Lam(_, _, None, _) -> failwith "every lambda should be typed"
  | Term.DB(_,id,i) ->
    (* ASSUMPTION : no clash in names variable as in \x\x.x*)
    let itx = instr_of_type (List.assoc id env) in
    let ix = (String (string_of_ident id)) in
    [ix]@itx@[Var;VarTerm]
  | _ -> Pp.print_term Format.std_formatter t; failwith "todo term"

let sample_1 =
  (mk_refl (mk_varTerm "x" (mk_varType "A")) (mk_varType "A"))

let sample_1_bis =
  (mk_refl (mk_varTerm "y" (mk_varType "A")) (mk_varType "A"))

let sample_2 =
  axiom_true

let sample_3 =
  axiom_and

let sample_4 =
  axiom_forall

let sample_5 =
  forall_intro "x" (mk_varType "A") sample_1

let sample_5_bis =
  forall_intro "y" (mk_varType "A") sample_1_bis

let sample_6 =
  axiom_impl

let sample_7 =
  forall_elim sample_5 (mk_absTerm (mk_var "x" (mk_varType "A")) sample_1.term) (mk_varType "A") (mk_varTerm "y" (mk_varType "A"))

let sample_7_bis =
  forall_elim sample_5_bis (mk_absTerm (mk_var "y" (mk_varType "B")) sample_1_bis.term) (mk_varType "B") (mk_varTerm "z" (mk_varType "B"))


let test = mk_thm (sample_7)

let version =
  [Int 6;Version]

let prelude : ast =
  let version = version in
  version@test

let equal ty tl tr =
  (load_const equal_str)@
  (type_arrow ty (type_arrow ty type_bool))@[ConstTerm]@
  tl@[AppTerm]@
  tr@[AppTerm]

let mk_prelude lc name =
  namespace := string_of_ident name;
  List.iter add_instr prelude

let define_hol_type id =
  begin
    add_instr (String (string_of_ident id));
    add_instr VarType
  end

let define_hol_const id te =
  let const = [(String (string_of_ident id))] in
  let term = instr_of_term [] te in
  List.iter add_instr (const@term@[DefineConst])

let mk_declaration lc id pty : unit =
  if is_hol_sort pty then
    define_hol_type id
  else
    failwith "todo declaration"


let mk_definable lc id pty : unit =
  match Env.declare_definable lc id pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  let ty =
    match pty_opt with
    | Some ty -> ty
    | None -> failwith "missing type in the definition"
  in
  if is_hol_type ty then
    define_hol_const id pte
  else
    failwith "todo definition"



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

let mk_ending () =
  Printf.printf "%s\n" (string_of_ast !cmds)
