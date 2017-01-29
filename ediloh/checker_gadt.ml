
(* ********************************* *)
(* Open theory stuff                 *)


type typeop
type cst
type var
type term
type thm
type ty

type empty
type ('a, 'b) succ = Succ

type 'a instr =
  | Empty : empty instr
  | String : string * 'a instr -> ((string, 'a) succ) instr
  | Int : int * 'a instr -> ((int, 'a) succ) instr
  | AbsTerm : (term, (var, 'a) succ) succ instr -> (term, 'a) succ instr
  | AbsThm : (thm, (var, 'a) succ) succ instr -> (thm, 'a) succ instr
  | AppTerm : (term, (term, 'a) succ) succ instr -> (term, 'a) succ instr
  | AppThm : (thm, (thm, 'a) succ) succ instr -> (thm, 'a) succ instr
  | Assume : (term,'a) succ instr -> (thm, 'a) succ instr
  | Axiom : (term, (term list, 'a) succ) succ instr -> (thm, 'a) succ instr
  | BetaConv : (term, 'a) succ instr -> (thm, 'a) succ instr
  | Cons : ('b list, ('b, 'a) succ) succ instr -> ('b list, 'a) succ instr
  | Const : (string, 'a) succ instr -> (cst, 'a) succ instr
  | ConstTerm : (ty, (cst, 'a) succ) succ instr -> (term, 'a) succ instr
  | DeductAntisym : (thm, (thm, 'a) succ) succ instr -> (thm, 'a) succ instr
  | Def : (int, 'a) succ instr -> ('b, 'a) succ instr
  | DefineConst : (term, (string, 'a) succ) succ instr -> (thm, (cst, 'a) succ) succ instr
  | DefineConstList : (thm, ((((string * var) list) list), 'a) succ) succ instr -> (thm, (cst list, 'a) succ) succ instr
  | DefineTypeOp : (thm, (string list, (string, (string, (string, 'a) succ) succ) succ) succ) succ instr -> (thm, (thm, (cst, (cst, (typeop, 'a) succ) succ) succ) succ) succ instr
  | EqMp : (thm, (thm, 'a) succ) succ instr -> (thm, 'a) succ instr
  | HdTl : ('b list, 'a) succ instr -> ('b list, ('b, 'a) succ) succ instr
  | Nil : 'a instr -> ('b list, 'a) succ instr
  | OpType : (ty list, (typeop, 'a) succ) succ instr -> (ty, 'a) succ instr
  | Pair : ('c, ('b, 'a) succ) succ instr -> ('b * 'c, 'a) succ instr
  | Pop : ('b, 'a) succ instr -> 'a instr
  | Pragma : ('b, 'a) succ instr -> 'a instr
  | ProveHyp : (thm, (thm, 'a) succ) succ instr -> (thm, 'a) succ instr
  | Ref : (int, 'a) succ instr -> ('b, 'a) succ instr
  | Refl : (term, 'a) succ instr -> (thm, 'a) succ instr
  | Remove : (int, 'a) succ instr -> ('b, 'a) succ instr
  | Subst : (thm, ((((string * ty) list) * (var * term) list), 'a) succ) succ instr -> (thm, 'a) succ instr
  | Sym : (thm, 'a) succ instr -> (thm, 'a) succ instr
  | Thm : (term, (term list, (thm, 'a) succ) succ) succ instr -> 'a instr
  | Trans : (thm, (thm, 'a) succ) succ instr -> (thm, 'a) succ instr
  | TypeOp : (string, 'a) succ instr -> (typeop, 'a) succ instr
  | Var : (ty, (string, 'a) succ) succ instr -> (var, 'a) succ instr
  | VarTerm : (var, 'a) succ instr -> (term, 'a) succ instr
  | VarType : (string, 'a) succ instr -> (ty, 'a) succ instr
  | Version : (int, 'a) succ instr -> 'a instr


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

let mk_Cons s t l = Cons(l(t(s)))

let mk_Const s str = Const(mk_String s str)

let mk_ConstTerm s cst ty = ConstTerm(ty(cst(s)))

let mk_ConstTerm2 s str ty = mk_ConstTerm s (fun s -> (mk_Const s str)) ty

let mk_DeductAntisym s thml thmr = DeductAntisym(thmr(thml(s)))

let mk_Def s i = Def(i(s))

let mk_Def2 s i = mk_Def s (fun s -> mk_Int s i)


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
  : (typeop, 'a) succ instr =
  TypeOp(String(str,s))

let type_bool
    (s : 'a instr)
  : (ty, 'a) succ instr =
  OpType(Nil(load_typeop s str_bool))

let type_arrow
    (s : 'a instr)
    (tyl : 'b instr -> (ty, 'b) succ instr)
    (tyr : 'c instr -> (ty, 'c) succ instr)
  : (ty, 'a) succ instr =
  OpType(Cons(Cons(Nil(tyr (tyl (load_typeop s str_arrow))))))

let type_true
    (s : 'a instr)
  : (ty, 'a) succ instr =
  type_bool s

let type_equal
    ( s : 'a instr)
    (tyl : 'b instr -> (ty, 'b) succ instr)
    (tyr : 'c instr -> (ty, 'c) succ instr)
  : (ty, 'a) succ instr =
  type_arrow s tyl (fun s -> type_arrow s tyr type_bool)

let type_and
    ( s : 'a instr)
  : (ty, 'a) succ instr =
  type_arrow s type_bool (fun s -> type_arrow s type_bool type_bool)

let type_impl
    ( s : 'a instr)
  : (ty, 'a) succ instr =
  type_arrow s type_bool (fun s -> type_arrow s type_bool type_bool)

let type_forall
    ( s : 'a instr)
    (ty : 'b instr -> (ty, 'b) succ instr)
  : (ty, 'a) succ instr =
  type_arrow s (fun s -> type_arrow s ty type_bool) type_bool

let const_of_str
    ( s : 'a instr)
    (str : string)
  : (cst, 'a) succ instr =
  Const(String(str,s))

let term_of_const
    ( s : 'a instr)
    (cst : ('b instr -> (cst, 'b) succ instr))
    (ty : ('c instr -> (ty, 'c) succ instr))
  : (term, 'a) succ instr =
  ConstTerm( ty (cst s))

let var_of_str
    ( s : 'a instr)
    ( str : string)
    (ty : 'b instr -> (ty, 'b) succ instr)
  : (var, 'a) succ instr =
  Var(ty(String(str,s)))

let term_of_var
    ( s : 'a instr)
    (var : ('b instr -> (var, 'b) succ instr))
  : (term, 'a) succ instr =
  VarTerm(var(s))

let term_true
    ( s : 'a instr)
  : (term, 'a) succ instr =
  term_of_const s (fun s -> const_of_str s str_true) type_true

let term_equal
    ( s : 'a instr)
    ( tl : 'b instr -> (term, 'b) succ instr)
    ( tr : 'c instr -> (term, 'c) succ instr)
    ( tyl : 'd instr -> (ty, 'd) succ instr)
    ( tyr : 'e instr -> (ty, 'e) succ instr)
  : (term, 'a) succ instr =
  let teq = term_of_const s (fun s -> const_of_str s str_equal) (fun s -> type_equal s tyl tyr) in
  AppTerm(tr(AppTerm(tl(teq))))

let term_and
    ( s : 'a instr)
    ( tl : 'b instr -> (term, 'b) succ instr)
    ( tr : 'c instr -> (term, 'c) succ instr)
  : (term, 'a) succ instr =
  let tand = term_of_const s (fun s -> const_of_str s str_and) (fun s -> type_equal s type_bool type_bool) in
  AppTerm(tr(AppTerm(tl(tand))))

let term_impl
    ( s : 'a instr)
    ( tl : 'b instr -> (term, 'b) succ instr)
    ( tr : 'b instr -> (term, 'b) succ instr)
  : (term, 'a) succ instr =
  let timpl = term_of_const s (fun s -> const_of_str s str_impl) (fun s -> type_equal s type_bool type_bool) in
  AppTerm(tr(AppTerm(tl(timpl))))

let term_forall
    ( s : 'a instr)
    ( te : 'b instr -> (term, 'b) succ instr)
    ( ty : 'c instr -> (ty, 'c) succ instr)
  : (term, 'a) succ instr =
  let tforall = term_of_const s (fun s -> const_of_str s str_forall) (fun s -> type_forall s ty) in
  AppTerm(te(tforall))

let axiom_true
    ( s : 'a instr)
  : (thm, 'a) succ instr =
  Axiom(term_true (Nil s))

let axiom_and
    ( s : 'a instr)
  : (thm, 'a) succ instr =
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
