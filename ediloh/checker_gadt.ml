
(* ********************************* *)
(* Open theory stuff                 *)

(* TODO : il faut encoder toutes les briques de bases en utilisant le polymorphisme façon système F sinon t'es mort ! *)


type typeop
type cst
type var
type term
type thm
type ty

type empty
type ('a, 'b) stack = Stack

type env_type = Name of string | Type of ty
type env_var = Var of var | Term of term
type env = EnvType of env_type | EnvVar of env_var

type 'a instr =
  | Empty : empty instr
  | String : string * 'a instr -> ((string, 'a) stack) instr
  | Int : int * 'a instr -> ((int, 'a) stack) instr
  | AbsTerm : (term, (var, 'a) stack) stack instr -> (term, 'a) stack instr
  | AbsThm : (thm, (var, 'a) stack) stack instr -> (thm, 'a) stack instr
  | AppTerm : (term, (term, 'a) stack) stack instr -> (term, 'a) stack instr
  | AppThm : (thm, (thm, 'a) stack) stack instr -> (thm, 'a) stack instr
  | Assume : (term,'a) stack instr -> (thm, 'a) stack instr
  | Axiom : (term, (term list, 'a) stack) stack instr -> (thm, 'a) stack instr
  | BetaConv : (term, 'a) stack instr -> (thm, 'a) stack instr
  | Cons : ('b list, ('b, 'a) stack) stack instr -> ('b list, 'a) stack instr
  | ConsType : (ty, (string, 'a) stack) stack instr -> (env_type list, 'a) stack instr
  | ConsVar : (term, (var, 'a) stack) stack instr -> (env_var list, 'a) stack instr
  | ConsEnv : (env_var list list, (env_type list list, 'a) stack) stack instr -> (env list, 'a) stack instr
  | Const : (string, 'a) stack instr -> (cst, 'a) stack instr
  | ConstTerm : (ty, (cst, 'a) stack) stack instr -> (term, 'a) stack instr
  | DeductAntisym : (thm, (thm, 'a) stack) stack instr -> (thm, 'a) stack instr
  | Def : (int, 'a) stack instr -> ('b, 'a) stack instr
  | DefineConst : (term, (string, 'a) stack) stack instr -> (thm, (cst, 'a) stack) stack instr
  | DefineConstList : (thm, ((((string * var) list) list), 'a) stack) stack instr -> (thm, (cst list, 'a) stack) stack instr
  | DefineTypeOp : (thm, (string list, (string, (string, (string, 'a) stack) stack) stack) stack) stack instr -> (thm, (thm, (cst, (cst, (typeop, 'a) stack) stack) stack) stack) stack instr
  | EqMp : (thm, (thm, 'a) stack) stack instr -> (thm, 'a) stack instr
  | HdTl : ('b list, 'a) stack instr -> ('b list, ('b, 'a) stack) stack instr
  | Nil : 'a instr -> ('b list, 'a) stack instr
  | OpType : (ty list, (typeop, 'a) stack) stack instr -> (ty, 'a) stack instr
  | Pair : ('c, ('b, 'a) stack) stack instr -> ('b * 'c, 'a) stack instr
  | Pop : ('b, 'a) stack instr -> 'a instr
  | Pragma : ('b, 'a) stack instr -> 'a instr
  | ProveHyp : (thm, (thm, 'a) stack) stack instr -> (thm, 'a) stack instr
  | Ref : (int, 'a) stack instr -> ('b, 'a) stack instr
  | Refl : (term, 'a) stack instr -> (thm, 'a) stack instr
  | Remove : (int, 'a) stack instr -> ('b, 'a) stack instr
  | Subst : (thm, ((((string * ty) list) * (var * term) list), 'a) stack) stack instr -> (thm, 'a) stack instr
  | Sym : (thm, 'a) stack instr -> (thm, 'a) stack instr
  | Thm : (term, (term list, (thm, 'a) stack) stack) stack instr -> 'a instr
  | Trans : (thm, (thm, 'a) stack) stack instr -> (thm, 'a) stack instr
  | TypeOp : (string, 'a) stack instr -> (typeop, 'a) stack instr
  | Var : (ty, (string, 'a) stack) stack instr -> (var, 'a) stack instr
  | VarTerm : (var, 'a) stack instr -> (term, 'a) stack instr
  | VarType : (string, 'a) stack instr -> (ty, 'a) stack instr
  | Version : (int, 'a) stack instr -> 'a instr

let mk_Empty = Empty

let mk_String s str = String(str,s)

let mk_Int s n = Int(n,s)

let mk_AbsTerm s v te = AbsTerm(te(v(s)))

let mk_AbsThm s v thm = AbsThm(thm(v(s)))

let mk_AppTerm s f t = AppTerm(t(f(s)))

let mk_AppThm s thmf thmt = AppThm(thmt(thmf(s)))

let mk_Assume s t = Assume(t(s))

let mk_Axiom s te hs = Axiom(te(hs(s)))

let mk_BetaConv s t = BetaConv(t(s))

let mk_Nil s = Nil(s)

let mk_Cons s x l = Cons(l(x(s)))

type ('c,'b) push = {push : 'a. 'a instr -> 'c -> ('b,'a) stack instr}

type 'b push_list = {push_list : 'a. ('a instr) -> ('b list, 'a) stack instr}

let rec mk_List
    (f: ('c,'b) push)
    (l:'c list)
   : 'b push_list =
  match l with
  | [] -> {push_list = mk_Nil}
  | x :: t ->
    let l' =  mk_List f t in
    let x' = (fun s -> f.push s x) in
    {push_list = (fun s -> Cons(l'.push_list(x'(s))))}

let mk_Const s str = Const(mk_String s str)

let mk_ConstTerm s cst ty = ConstTerm(ty(cst(s)))

let mk_ConstTerm2 s str ty = mk_ConstTerm s (fun s -> (mk_Const s str)) ty

let mk_DeductAntisym s thml thmr = DeductAntisym(thmr(thml(s)))

let mk_Def s i = Def(i(s))

let mk_Def2 s i = mk_Def s (fun s -> mk_Int s i)

let mk_DefineConst s str te = DefineConst(te(mk_String s str))

let mk_DefineConstList s thm l  = DefineConstList(thm(l(s)))



type 'a test

let string_of_instr: type a. a instr -> string =
  let rec string_of_instr : type a. (string -> string) -> a instr -> string = fun k -> fun l ->
    match l with
    | Empty -> ""
    | String(s,l) -> string_of_instr (fun str -> k (str^(Printf.sprintf "\"%s\"\n" s))) l
    | Int(i,l) -> string_of_instr (fun str -> k (str^(string_of_int i)^"\n")) l
    | AbsTerm l -> string_of_instr (fun str -> k (str^"absTerm\n")) l
    | AbsThm l -> string_of_instr (fun str -> k (str^"absThm\n")) l
    | AppTerm l -> string_of_instr (fun str -> k (str^"appTerm\n")) l
    | AppThm l -> string_of_instr (fun str -> k (str^"appThm\n")) l
    | Axiom l -> string_of_instr (fun str -> k (str^"axiom\n")) l
    | Assume l -> string_of_instr (fun str -> k (str^"assume\n")) l
    | BetaConv l -> string_of_instr (fun str -> k (str^"betaConv\n")) l
    | Cons l -> string_of_instr (fun str -> k (str^"cons\n")) l
    (* TODO : fixme *)
    | ConsType l -> string_of_instr (fun str -> k (str^"nil\ncons\ncons\n")) l
    | ConsVar l -> string_of_instr (fun str -> k (str^"nil\ncons\ncons\n")) l
    | ConsEnv l -> string_of_instr (fun str -> k (str^"nil\ncons\ncons\n")) l
    (* ENDTODO *)
    | Const l -> string_of_instr (fun str -> k (str^"const\n")) l
    | ConstTerm l -> string_of_instr (fun str -> k (str^"constTerm\n")) l
    | DeductAntisym l -> string_of_instr (fun str -> k (str^"deductAntisym\n")) l
    | Def l -> string_of_instr (fun str -> k (str^"def\n")) l
    | DefineConst l -> string_of_instr (fun str -> k (str^"defineConst\n")) l
    | DefineConstList l -> string_of_instr (fun str -> k (str^"defineConstList\n")) l
    | DefineTypeOp l -> string_of_instr (fun str -> k (str^"defineTypeOp\n")) l
    | EqMp l -> string_of_instr (fun str -> k (str^"eqMp\n")) l
    | HdTl l -> string_of_instr (fun str -> k (str^"hdTl\n")) l
    | Nil l -> string_of_instr (fun str -> k (str^"nil\n")) l
    | OpType l -> string_of_instr (fun str -> k (str^"opType\n")) l
    | Pair l -> string_of_instr (fun str -> k (str^"nil\ncons\ncons\n")) l
    | Pop l -> string_of_instr (fun str -> k (str^"pop\n")) l
    | Pragma l -> string_of_instr (fun str -> k (str^"pragma\n")) l
    | ProveHyp l -> string_of_instr (fun str -> k (str^"proveHyp\n")) l
    | Ref l -> string_of_instr (fun str -> k (str^"ref\n")) l
    | Refl l -> string_of_instr (fun str -> k (str^"refl\n")) l
    | Remove l -> string_of_instr (fun str -> k (str^"remove\n")) l
    | Subst l -> string_of_instr (fun str -> k (str^"subst\n")) l
    | Sym l -> string_of_instr (fun str -> k (str^"sym\n")) l
    | Thm l -> string_of_instr (fun str -> k (str^"thm\n")) l
    | Trans l -> string_of_instr (fun str -> k (str^"trans\n")) l
    | TypeOp l -> string_of_instr (fun str -> k (str^"typeOp\n")) l
    | Var l -> string_of_instr (fun str -> k (str^"var\n")) l
    | VarTerm l -> string_of_instr (fun str -> k (str^"varTerm\n")) l
    | VarType l -> string_of_instr (fun str -> k (str^"varType\n")) l
    | Version l -> string_of_instr (fun str -> k (str^"version\n")) l
  in
  fun l -> string_of_instr (fun k -> k) l


let str_forall = "!"

let str_equal = "="

let str_true = "T"

let str_arrow = "->"

let str_bool = "bool"

let str_impl = "==>"

let str_and = "/\\\\"

let str_or = "\\\\/"

let str_debug = "debug"

let debug
    (s : 'a instr)
  : 'a instr =
  Pragma(String(str_debug,s))

let load_typeop
    (s : 'a instr)
    (str : string)
  : (typeop, 'a) stack instr =
  TypeOp(String(str,s))

let type_bool
    (s : 'a instr)
  : (ty, 'a) stack instr =
  OpType(Nil(load_typeop s str_bool))

let type_arrow
    (s : 'a instr)
    (tyl : 'b instr -> (ty, 'b) stack instr)
    (tyr : 'c instr -> (ty, 'c) stack instr)
  : (ty, 'a) stack instr =
  OpType(Cons(Cons(Nil(tyr (tyl (load_typeop s str_arrow))))))

let type_true
    (s : 'a instr)
  : (ty, 'a) stack instr =
  type_bool s

let type_equal
    ( s : 'a instr)
    (tyl : 'b instr -> (ty, 'b) stack instr)
    (tyr : 'c instr -> (ty, 'c) stack instr)
  : (ty, 'a) stack instr =
  type_arrow s tyl (fun s -> type_arrow s tyr type_bool)

let type_and
    ( s : 'a instr)
  : (ty, 'a) stack instr =
  type_arrow s type_bool (fun s -> type_arrow s type_bool type_bool)

let type_impl
    ( s : 'a instr)
  : (ty, 'a) stack instr =
  type_arrow s type_bool (fun s -> type_arrow s type_bool type_bool)

let type_forall
    ( s : 'a instr)
    (ty : 'b instr -> (ty, 'b) stack instr)
  : (ty, 'a) stack instr =
  type_arrow s (fun s -> type_arrow s ty type_bool) type_bool

let const_of_str
    ( s : 'a instr)
    (str : string)
  : (cst, 'a) stack instr =
  Const(String(str,s))

let term_of_const
    ( s : 'a instr)
    (cst : ('b instr -> (cst, 'b) stack instr))
    (ty : ('c instr -> (ty, 'c) stack instr))
  : (term, 'a) stack instr =
  ConstTerm( ty (cst s))

let var_of_str
    ( s : 'a instr)
    ( str : string)
    (ty : 'b instr -> (ty, 'b) stack instr)
  : (var, 'a) stack instr =
  Var(ty(String(str,s)))

let term_of_var
    ( s : 'a instr)
    (var : ('b instr -> (var, 'b) stack instr))
  : (term, 'a) stack instr =
  VarTerm(var(s))

let term_true
    ( s : 'a instr)
  : (term, 'a) stack instr =
  term_of_const s (fun s -> const_of_str s str_true) type_true

let term_equal
    ( s : 'a instr)
    ( tl : 'b instr -> (term, 'b) stack instr)
    ( tr : 'c instr -> (term, 'c) stack instr)
    ( tyl : 'd instr -> (ty, 'd) stack instr)
    ( tyr : 'e instr -> (ty, 'e) stack instr)
  : (term, 'a) stack instr =
  let teq = term_of_const s (fun s -> const_of_str s str_equal) (fun s -> type_equal s tyl tyr) in
  AppTerm(tr(AppTerm(tl(teq))))

let term_and
    ( s : 'a instr)
    ( tl : 'b instr -> (term, 'b) stack instr)
    ( tr : 'c instr -> (term, 'c) stack instr)
  : (term, 'a) stack instr =
  let tand = term_of_const s (fun s -> const_of_str s str_and) (fun s -> type_equal s type_bool type_bool) in
  AppTerm(tr(AppTerm(tl(tand))))

let term_impl
    ( s : 'a instr)
    ( tl : 'b instr -> (term, 'b) stack instr)
    ( tr : 'b instr -> (term, 'b) stack instr)
  : (term, 'a) stack instr =
  let timpl = term_of_const s (fun s -> const_of_str s str_impl) (fun s -> type_equal s type_bool type_bool) in
  AppTerm(tr(AppTerm(tl(timpl))))

let term_forall
    ( s : 'a instr)
    ( te : 'b instr -> (term, 'b) stack instr)
    ( ty : 'c instr -> (ty, 'c) stack instr)
  : (term, 'a) stack instr =
  let tforall = term_of_const s (fun s -> const_of_str s str_forall) (fun s -> type_forall s ty) in
  AppTerm(te(tforall))

let axiom_true
    ( s : 'a instr)
  : (thm, 'a) stack instr =
  Axiom(term_true (Nil s))

let axiom_and
    ( s : 'a instr)
  : (thm, 'a) stack instr =
  let x = (fun s -> term_of_var s (fun s -> var_of_str s "x" type_bool)) in
  let y = (fun s -> term_of_var s (fun s -> var_of_str s "y" type_bool)) in
  let l = (fun s -> term_and s x y) in
  let vf = (fun s -> var_of_str s "f" (fun s -> type_arrow s type_bool type_bool)) in
  let tf = (fun s -> VarTerm(vf(s))) in
  let rl' = (fun s -> AppTerm(y(AppTerm(x(tf(s)))))) in
  let rl = (fun s -> AbsTerm(rl'(vf(s)))) in
  let rr' = (fun s -> AppTerm(term_true(AppTerm(term_true(tf(s)))))) in
  let rr = (fun s -> AbsTerm(rr'(vf(s)))) in
  let tyr = (fun s -> (type_arrow s (fun s -> type_arrow s type_bool ((fun s -> type_arrow s type_bool type_bool))) type_bool)) in
  let r = (fun s -> term_equal s rl rr tyr tyr) in
  let te = (fun s -> term_equal s l r type_bool type_bool) in
  Axiom(te (Nil s))

let axiom_impl
    ( s : 'a instr)
  : (thm, 'a) stack instr =
  let x = (fun s -> term_of_var s (fun s -> var_of_str s "x" type_bool)) in
  let y = (fun s -> term_of_var s (fun s -> var_of_str s "x" type_bool)) in
  let l = (fun s -> term_impl s x y) in
  let r = (fun s -> term_equal s (fun s -> term_and s x y) (fun s -> x(s)) type_bool type_bool) in
  let te = (fun s -> term_equal s l r type_bool type_bool) in
  Axiom(te (Nil s))

let axiom_forall
    ( s : 'a instr)
  : (thm, 'a) stack instr =
  let ty = (fun s -> VarType(String("A",s))) in
  let p = (fun s -> term_of_var s (fun s -> var_of_str s "p" (fun s -> type_forall s ty))) in
  let l = (fun s -> term_forall s p ty) in
  let r = (fun s -> term_equal s p (fun s -> (AbsTerm(term_true(var_of_str s "x" ty)))) ty ty) in
  let te = (fun s -> term_equal s l r ty ty) in
  Axiom(te (Nil s))

type 'a push_type = {push_type: 'a.'a instr -> (ty, 'a) stack instr}
type 'a push_var = {push_var: 'a. 'a instr -> (var, 'a) stack instr}
type 'a push_term = {push_term: 'a. 'a instr -> (term, 'a) stack instr}

let subst (s: 'd instr) ((env_type,env_var):((string * 'a push_type) list * ('b push_var * 'c push_term) list))
  : (env list, 'd) stack instr =
  let push_env_type = {push = fun s -> fun (str,ty) -> ConsType(ty.push_type(String(str,s)))} in
  let env_type' = mk_List push_env_type env_type in
  let push_env_var = {push = fun s -> fun (var,te) -> ConsVar(te.push_term(var.push_var(s)))} in
  let env_var' = mk_List push_env_var env_var in
  ConsEnv(env_var'.push_list((env_type'.push_list(s))))

let forall_intro
    ( s : 'a instr)
    ( ty : 'b push_type)
    ( proof : 'c instr ->  (thm, 'c) stack instr)
    ( term : 'd instr -> (term, 'd) stack instr)
  : (thm, 'a) stack instr =
  let l = (fun s -> AbsThm(DeductAntisym(axiom_true(proof((var_of_str s "x" ty.push_type)))))) in
  let r = (fun s -> axiom_forall(s)) in
  let x = {push_var = (fun s -> var_of_str s "x" (type_arrow s ty.push_type type_bool))} in
  let t = {push_term = (fun s -> var_of_str s "v"
  let sigma = (fun s -> subst s (["A";ty],[x;
