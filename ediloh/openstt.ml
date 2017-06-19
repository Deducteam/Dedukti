module type OpenSTT = sig

  type name
  type var
  type tyOp
  type ty
  type term
  type const

  type hyp
  type thm

  type 'a obj

  val mk_name : string list -> string -> name obj


  val mk_var : name obj -> ty obj -> var obj


  val mk_tyOp : name obj -> tyOp obj


  val mk_varType : name obj -> ty obj

  val ty_of_tyOp : tyOp obj -> (ty obj) list -> ty obj

  val mk_arrow_type : ty obj -> ty obj -> ty obj

  val mk_bool_type : ty obj

  val mk_equal_type : ty obj -> ty obj

  val mk_impl_type : ty obj

  val mk_forall_type : ty obj -> ty obj


  val mk_app_term : term obj -> term obj -> term obj

  val mk_abs_term : var obj -> term obj -> term obj

  val mk_var_term : var obj -> term obj

  val term_of_const : const obj -> ty obj -> term obj

  val mk_equal_term : term obj -> term obj -> ty obj -> term obj

  val mk_impl_term : term obj -> term obj -> term obj

  val mk_forall_term : term obj -> ty obj -> term obj

  val const_of_name : name obj -> const obj

  val mk_subst : thm obj -> (name obj * ty obj) list -> (var obj * term obj) list -> thm obj

  val mk_hyp : (term obj) list -> hyp obj


  val mk_axiom : hyp obj -> term obj -> thm obj

  val mk_eqMp : thm obj -> thm obj -> thm obj

  val mk_appThm : thm obj -> thm obj -> thm obj

  val mk_absThm : var obj -> thm obj -> thm obj

  val mk_rule_intro_forall : name obj-> ty obj -> term obj -> thm obj -> thm obj

  val mk_rule_elim_forall : thm obj -> term obj -> ty obj -> term obj -> thm obj

  val mk_rule_intro_impl : thm obj -> term obj -> term obj -> thm obj

  val mk_rule_elim_impl : thm obj -> thm obj -> term obj -> term obj -> thm obj

  val mk_impl_equal : thm obj -> thm obj -> term obj -> term obj -> term obj -> term obj -> thm obj

  val mk_forall_equal : thm obj -> name obj -> term obj -> term obj -> ty obj -> thm obj

  val mk_equal_equal : term obj -> term obj -> term obj -> term obj -> thm obj -> thm obj -> ty obj -> thm obj

  val thm_of_const_name : name obj -> thm obj

  val mk_refl : term obj -> thm obj

  val mk_assume : term obj -> thm obj

  val mk_const : name obj -> term obj -> unit

  val mk_sym : thm obj -> thm obj

  val mk_betaConv : term obj -> thm obj

  val mk_thm : name obj -> term obj -> hyp obj -> thm obj -> unit

  val thm_of_lemma : name obj -> thm obj

  val debug : 'a obj -> unit

  val comment : ('a, Format.formatter, unit) format -> 'a
end

module Basic = struct
  type name
  type num
  type tyOp
  type ty
  type const
  type var
  type term
  type thm

  type hyp = term list

  type _ op =
    | Empty : unit op
    | Int : int * 'b op -> (int * 'b) op
    | String : string * 'b op -> (name * 'b) op
    | Nil : 'b op -> ('a list * 'b) op
    | Cons : ('a list * ('a * 'b)) op -> ('a list * 'b) op
    | Pair : ('a * ('c * 'b)) op -> (('c * 'a) * 'b) op
    | Const : (name * 'b) op -> (const * 'b) op
    | Def : (int * ('b * 'a)) op -> ('b * 'a) op
    | OpType : (ty list * (tyOp * 'a)) op -> (ty * 'a) op
    | AppTerm : (term * (term * 'a)) op -> (term * 'a) op
    | AbsTerm : (term * (var * 'a)) op -> (term * 'a) op
    | ConstTerm : (ty * (const * 'a)) op -> (term * 'a) op
    | DefineConst : (term * (name * 'a)) op -> (thm * (const * 'a)) op
    | Pop : ('b * 'a) op -> 'a op
    | Remove : (int * 'b) op -> ('a * 'b) op
    | Pragma : (name * 'a) op -> 'a op
    | TypeOp : (name * 'b) op -> (tyOp * 'b) op
    | Var : (ty * (name * 'b)) op -> (var * 'b) op
    | VarTerm : (var * 'b) op -> (term * 'b) op
    | VarType : (name * 'b) op -> (ty * 'b) op
    | Ref : (int * 'b) op -> ('a * 'b) op
    | Version : (int * 'b) op -> 'b op
    | Axiom : (term * (term list * 'a)) op -> (thm * 'a) op
    | Refl : (term * 'b) op -> (thm * 'b) op
    | AppThm : (thm * (thm * 'b)) op -> (thm * 'b) op
    | AbsThm : (thm * (var * 'a)) op -> (thm * 'a) op
    | Sym : (thm * 'b) op -> (thm * 'b) op
    | BetaConv : (term * 'b) op -> (thm * 'b) op
    | Trans : (thm * (thm * 'b)) op -> (thm * 'b) op
    | Subst : (thm * (((name * ty) list * (var * term) list) * 'b)) op -> (thm * 'b) op
    | EqMp : (thm * (thm * 'b)) op -> (thm * 'b) op
    | Assume : (term * 'b) op -> (thm * 'b) op
    | DeductAntiSym : (thm * (thm * 'b)) op -> (thm * 'b) op
    | ProveHyp : (thm * (thm * 'b)) op -> (thm * 'b) op
    | Thm : (term * (term list * (thm * 'b))) op -> 'b op

  let rec print_op : type a. _ -> a op -> unit =
    fun out op ->
      match op with
      | Empty -> Format.fprintf out ""
      | Int(i,op) -> print_op out op; Format.fprintf out "%d@." i
      | String(s,op) ->  print_op out op; Format.fprintf out "\"%s\"@." s
      | Nil(op) ->  print_op out op; Format.fprintf out "nil@."
      | Cons(op) ->  print_op out op; Format.fprintf out "cons@."
      | Pair(op) -> print_op out op; Format.fprintf out "nil@.cons@.cons@."
      | Const(op) ->  print_op out op; Format.fprintf out "const@."
      | Def(op) -> print_op out op; Format.fprintf out "def@."
      | OpType(op) -> print_op out op; Format.fprintf out "opType@."
      | AppTerm(op) -> print_op out op; Format.fprintf out "appTerm@."
      | AbsTerm(op) -> print_op out op; Format.fprintf out "absTerm@."
      | ConstTerm(op) -> print_op out op; Format.fprintf out "constTerm@."
      | DefineConst(op) -> print_op out op; Format.fprintf out "defineConst@."
      | Pop(op) -> print_op out op; Format.fprintf out "pop@."
      | Remove(op) -> print_op out op; Format.fprintf out "remove@."
      | Pragma(op) -> print_op out op; Format.fprintf out "pragma@."
      | TypeOp(op) -> print_op out op; Format.fprintf out "typeOp@."
      | Var(op) -> print_op out op; Format.fprintf out "var@."
      | VarTerm(op) -> print_op out op; Format.fprintf out "varTerm@."
      | VarType(op) -> print_op out op; Format.fprintf out "varType@."
      | Ref(op) -> print_op out op; Format.fprintf out "ref@."
      | Version(op) -> print_op out op; Format.fprintf out "version@."
      | Axiom(op) -> print_op out op; Format.fprintf out "axiom@."
      | Refl(op) -> print_op out op; Format.fprintf out "refl@."
      | AppThm(op) -> print_op out op; Format.fprintf out "appThm@."
      | AbsThm(op) -> print_op out op; Format.fprintf out "absThm@."
      | Sym(op) -> print_op out op; Format.fprintf out "sym@."
      | BetaConv(op) -> print_op out op; Format.fprintf out "betaConv@."
      | Trans(op) -> print_op out op; Format.fprintf out "trans@."
      | Subst(op) -> print_op out op; Format.fprintf out "subst@."
      | EqMp(op) -> print_op out op; Format.fprintf out "eqMp@."
      | Assume(op) -> print_op out op; Format.fprintf out "assume@."
      | DeductAntiSym(op) -> print_op out op; Format.fprintf out "deductAntisym@."
      | ProveHyp(op) -> print_op out op; Format.fprintf out "proveHyp@."
      | Thm(op) -> print_op out op; Format.fprintf out "thm@."

  let version = print_op Format.std_formatter (Version(Int(6,Empty)))

  type 'a push =
    {
      push: 'b. 'b op -> ('a * 'b) op
    }

  let print_push out push =
    print_op out (push.push Empty)

  type 'a obj =
    {
      instr: 'a push;
      id: int;
    }


  let counter = ref 0

  type const_defined = (string, thm obj * const obj) Hashtbl.t

  let const_defined:const_defined = Hashtbl.create 87

  let lemmas_defined = Hashtbl.create 87

  let load (t:'a obj) (k:'b op) : ('a *'b) op =
    Ref(Int(t.id,k))

  type hash_op = Hash : _ op -> hash_op

  let memory : type a. (hash_op,int) Hashtbl.t = Obj.magic @@ Hashtbl.create 87

  let save (t:'a push) : 'a obj =
    if Hashtbl.mem memory (Hash (t.push Empty)) then
      {instr=t; id = Hashtbl.find memory (Hash(t.push Empty))}
    else
      let id = !counter in incr counter;
      let to_def obj = Pop(Def(Int(id,(obj.push Empty)))) in
      print_op Format.std_formatter (to_def t);
      Hashtbl.add memory (Hash(t.push Empty)) id;
      {instr=t;id}


  let mk_name namespace name =
    let mk_namespace md = List.fold_left (fun s x -> s^x^".") "" md in
    let full_name = (mk_namespace namespace)^name in
    save { push = fun k -> String(full_name,k)}

  let arrow_name = mk_name [] "->"

  let bool_name = mk_name [] "bool"

  let equal_name = mk_name [] "="

  let impl_name = mk_name [] "==>"

  let forall_name = mk_name [] "!"

  (* FIXME: can bypass this hack by changin hashtbl so that the keys are of types name * 'a op *)
  let string_of_name name =
    match name.instr.push Empty with
    | String(s,Empty) -> s
    | _ -> assert false

  let _ =
    Hashtbl.add const_defined (string_of_name equal_name)
      (save { push = fun k -> Obj.magic@@ String("Error",k)} , save {push=fun k -> Const(load equal_name k)})


  let mk_var name ty : var obj =
    save {push = fun k -> Var(load ty (load name k))}

  let mk_tyOp name : tyOp obj =
    save {push = fun k -> TypeOp(load name k)}

  let arrow_tyOp = mk_tyOp arrow_name

  let bool_tyOp = mk_tyOp bool_name

  let mk_varType name : ty obj =
    save {push = fun k -> VarType(load name k)}

  let mk_list l =
    let rec mk_list : type b. 'a obj list ->  b op ->('a list * b) op =
      fun l k ->
        match l with
        | [] -> Nil k
        | x::t ->
          Cons (mk_list t (load x k))
    in
    save {push = fun k -> mk_list l k}

  let ty_of_tyOp (tyOp: tyOp obj) (tys:ty obj list) : ty obj =
    let list = mk_list tys in
    save {push = fun k -> OpType(load list (load tyOp k))}

  let mk_arrow_type tyl tyr : ty obj =
    ty_of_tyOp arrow_tyOp [tyl;tyr]

  let mk_bool_type : ty obj =
    ty_of_tyOp bool_tyOp []

  let mk_equal_type ty : ty obj =
    mk_arrow_type ty (mk_arrow_type ty mk_bool_type)

  let mk_impl_type =
    mk_arrow_type mk_bool_type (mk_arrow_type mk_bool_type mk_bool_type)

  let mk_forall_type ty =
    mk_arrow_type (mk_arrow_type ty mk_bool_type) mk_bool_type

  let mk_app_term f t : term obj =
    save {push = fun k -> AppTerm(load t (load f k))}

  let mk_abs_term v t : term obj =
    save {push = fun k -> AbsTerm(load t (load v k))}

  let mk_var_term v =
    save { push = fun k -> VarTerm(load v k)}

  let term_of_const cst ty =
    save {push = fun k -> ConstTerm(load ty (load cst k))}

  let const_of_name name =
    let str = string_of_name name in
    if Hashtbl.mem const_defined str then
      (snd @@ Hashtbl.find const_defined str)
    else
      (* failwith (Format.sprintf "Const %s not found" str) *)
      save {push = fun k -> Const(load name k)}


  let mk_equal_term left right ty =
    let ty = mk_equal_type ty in
    let cst = term_of_const (const_of_name equal_name) ty in
    mk_app_term (mk_app_term cst left) right

  let mk_const name (term:term obj) : unit =
    let str =
      match name.instr.push Empty with
      | String(s,Empty) -> s
      | _ -> assert false
    in
    let thm = !counter in
    let cst = incr counter; !counter in
    incr counter;
    let push (k:'a op) = DefineConst(load term (load name k)) in
    let def_thm k = Pop(Def(Int(thm,k))) in
    let def_cst k = Pop(Def(Int(cst,k))) in
    print_op Format.std_formatter (def_cst @@ def_thm @@ push Empty);
    let push_thm = {push=fun k -> Ref(Int(thm,k))} in
    let push_cst = {push=fun k -> Ref(Int(cst,k))} in
    let obj_thm = {instr=push_thm; id = thm} in
    let obj_cst = {instr=push_cst; id = cst} in
    Hashtbl.add const_defined str (obj_thm,obj_cst)

  let debug obj =
    print_op Format.std_formatter (Pop(Pragma(String("debug",load obj Empty))))

  let comment fmt =
    Format.fprintf Format.std_formatter fmt

  let mk_hyp ts : hyp obj = mk_list ts

  let mk_axiom hyp term =
    save {push = fun k -> Axiom(load term (load hyp k))}

  let thm_of_const_name name =
    let str = string_of_name name in
    if Hashtbl.mem const_defined str then
      (fst @@ Hashtbl.find const_defined str)
    else
      failwith (Format.sprintf "Const %s not found" str)

  let mk_refl term =
    save {push = fun k -> Refl(load term k)}

  let mk_appThm thml thmr =
    save {push = fun k -> AppThm(load thmr (load thml k))}

  let mk_absThm var thm =
    save {push = fun k -> AbsThm(load thm (load var k))}

  let mk_sym thm =
    save { push = fun k -> Sym(load thm k)}

  let mk_betaConv term =
    save { push = fun k -> BetaConv(load term k)}

  let mk_trans thml thmr =
    save { push = fun k -> Trans(load thmr (load thml k))}

  let mk_subst (thm: thm obj) (env_ty: (name obj * ty obj) list) (env_var: (var obj * term obj) list) =
    let to_pair (left,right) = save {push = fun k -> Pair(load right (load left k))} in
    let env_ty = mk_list (List.map to_pair env_ty) in
    let env_var = mk_list (List.map to_pair env_var) in
    let subst = to_pair (env_ty,env_var) in
    save {push = fun k -> Subst(load thm (load subst k))}

  let mk_eqMp thml thmr =
    save { push = fun k -> EqMp(load thml (load thmr k))}

  let mk_proveHyp thml thmr =
    save { push = fun k -> ProveHyp(load thml(load thmr k))}

  let mk_assume term =
    save { push = fun k -> Assume(load term k)}

  let mk_deductAntiSym thml thmr =
    save { push = fun k -> DeductAntiSym(load thmr (load thml k))}

  let mk_thm name term hyp thm =
    let str = string_of_name name in
    Hashtbl.add lemmas_defined str thm;
    print_op Format.std_formatter (Thm(load term (load hyp (load thm Empty))))

  let mk_remove x =
    print_op Format.std_formatter (Pop(Remove(Int(x,Empty))))

  let thm_of_lemma name =
    let str = string_of_name name in
    if Hashtbl.mem lemmas_defined str then
      Hashtbl.find lemmas_defined str
    else
      failwith (Format.sprintf "Lemma %s not found" (string_of_name name))
end

module OpenTheory = struct
  include Basic

  let true_name = mk_name [] "T"

  let and_name = mk_name [] "/\\\\"

  let _ =
    Hashtbl.add const_defined (string_of_name true_name)
      (save { push = fun k -> Obj.magic@@ String("Error",k)} , save {push=fun k -> Const(load true_name k)});
    Hashtbl.add const_defined (string_of_name and_name)
      (save { push = fun k -> Obj.magic@@ String("Error",k)} , save {push=fun k -> Const(load and_name k)});
    Hashtbl.add const_defined (string_of_name impl_name)
      (save { push = fun k -> Obj.magic@@ String("Error",k)} , save {push=fun k -> Const(load impl_name k)});
    Hashtbl.add const_defined (string_of_name forall_name)
      (save { push = fun k -> Obj.magic@@ String("Error",k)} , save {push=fun k -> Const(load forall_name k)})

  let mk_true_type = mk_bool_type

  let mk_and_type = mk_arrow_type mk_bool_type (mk_arrow_type mk_bool_type mk_bool_type)

  let mk_true_term = term_of_const (const_of_name true_name) mk_true_type

  let mk_and_term left right =
    let cst = term_of_const (const_of_name and_name) mk_and_type in
    mk_app_term (mk_app_term cst left) right

  let mk_impl_term left right =
    let cst = term_of_const (const_of_name impl_name) mk_impl_type in
    mk_app_term (mk_app_term cst left) right

  let mk_forall_term f ty =
    let cst = term_of_const (const_of_name forall_name) (mk_forall_type ty) in
    mk_app_term cst f

  let mk_axiom_true = mk_axiom (mk_hyp []) mk_true_term

  let mk_axiom_and_ =
    let binop_type = mk_arrow_type mk_bool_type (mk_arrow_type mk_bool_type mk_bool_type) in
    let vx = mk_var (mk_name [] "x") mk_bool_type in
    let vy = mk_var (mk_name [] "y") mk_bool_type in
    let x = mk_var_term vx in
    let y = mk_var_term vy in
    let tand = mk_abs_term vx (mk_abs_term vy (mk_and_term x y)) in
    let vf = mk_var (mk_name [] "f") binop_type in
    let f = mk_var_term vf in
    let rl = mk_abs_term vf (mk_app_term (mk_app_term f x) y) in
    let rr = mk_abs_term vf (mk_app_term (mk_app_term f mk_true_term) mk_true_term) in
    let rhs = mk_abs_term vx (mk_abs_term vy (mk_equal_term rl rr
                                                (mk_arrow_type binop_type mk_bool_type))) in
    let term = mk_equal_term tand rhs binop_type in
    mk_axiom (mk_hyp []) term


  (* proof of tl'=tr' with tl ->B tl' et tr ->B tr' and thm a proof of tl = tr *)
  let beta_equal thm tl tr =
    let left = mk_sym (mk_betaConv tl) in
    let right = mk_betaConv tr in
    let trans = mk_trans left thm in
    mk_trans trans right

  let mk_axiom_and left right =
    let binop_type = mk_arrow_type mk_bool_type (mk_arrow_type mk_bool_type mk_bool_type) in
    let vx = mk_var (mk_name [] "freshx") mk_bool_type in
    let vy = mk_var (mk_name [] "freshy") mk_bool_type in
    let x = mk_var_term vx in
    let y = mk_var_term vy in
    let t1 = (mk_abs_term vy (mk_and_term x y)) in
    let fv = mk_var (mk_name [] "freshf") (binop_type) in
    let ft = mk_var_term fv in
    let rl = mk_abs_term fv (mk_app_term (mk_app_term ft x) y) in
    let rr = mk_abs_term fv (mk_app_term (mk_app_term ft mk_true_term) mk_true_term) in
    let t2 = mk_abs_term vy (mk_equal_term rl rr (mk_arrow_type binop_type mk_bool_type)) in
    let refl = mk_refl left in
    let tl = mk_app_term (mk_abs_term vx t1) left in
    let tr = mk_app_term (mk_abs_term vx t2) left in
    let beta1 = beta_equal (mk_appThm mk_axiom_and_ refl) tl tr in
    let rl = mk_abs_term fv (mk_app_term (mk_app_term ft left) y) in
    let t2 = mk_equal_term rl rr (mk_arrow_type binop_type mk_bool_type) in
    let refl = mk_refl right in
    let tl = mk_app_term (mk_abs_term vy (mk_and_term left y)) right in
    let tr = mk_app_term (mk_abs_term vy t2) right in
    beta_equal (mk_appThm beta1 refl) tl tr

  let mk_axiom_impl_ =
    let binop_type = mk_arrow_type mk_bool_type (mk_arrow_type mk_bool_type mk_bool_type) in
    let vx = mk_var (mk_name [] "x") mk_bool_type in
    let vy = mk_var (mk_name [] "y") mk_bool_type in
    let x = mk_var_term vx in
    let y = mk_var_term vy in
    let implt = mk_abs_term vx (mk_abs_term vy (mk_impl_term x y)) in
    let r = mk_equal_term (mk_and_term x y) x mk_bool_type in
    let term = mk_equal_term implt (mk_abs_term vx (mk_abs_term vy r)) binop_type in
    mk_axiom (mk_hyp []) term

  let mk_axiom_impl left right =
    let vx = mk_var (mk_name [] "freshx") mk_bool_type in
    let vy = mk_var (mk_name [] "freshy") mk_bool_type in
    let x = mk_var_term vx in
    let y = mk_var_term vy in
    let tl = mk_app_term (mk_abs_term vx (mk_abs_term vy (mk_impl_term x y))) left in
    let tr = mk_app_term (mk_abs_term vx (mk_abs_term vy
                                            (mk_equal_term (mk_and_term x y) x mk_bool_type))) left in
    let refl = mk_refl left in
    let beta1 = beta_equal (mk_appThm mk_axiom_impl_ refl) tl tr in
    let tl = mk_app_term (mk_abs_term vy (mk_impl_term left y)) right in
    let tr = mk_app_term
        (mk_abs_term vy (mk_equal_term (mk_and_term left y) left mk_bool_type)) right in
    let refl = mk_refl right in
    let thm = beta_equal (mk_appThm beta1 refl) tl tr in
    thm



  let mk_axiom_forall_ =
    let ty = mk_varType (mk_name [] "A") in
    let vx = mk_var (mk_name [] "x") (mk_arrow_type ty mk_bool_type) in
    let x = mk_var_term vx in
    let tforall = mk_abs_term vx (mk_forall_term x ty) in
    let r = mk_abs_term (mk_var (mk_name [] "v") ty) mk_true_term in
    let instr = mk_equal_term tforall (mk_abs_term vx (mk_equal_term x r (mk_arrow_type ty mk_bool_type))) (mk_arrow_type (mk_arrow_type ty mk_bool_type) mk_bool_type) in
    mk_axiom (mk_hyp []) instr

  let mk_axiom_forall p ty =
    let env_type = [(mk_name [] "A"),ty] in
    let env_var = [] in
    let var = mk_var (mk_name [] "x") (mk_arrow_type ty mk_bool_type) in
    let tl = mk_app_term (mk_abs_term var (mk_forall_term (mk_var_term var) ty)) p in
    let tr = mk_app_term (mk_abs_term var (mk_equal_term (mk_var_term var) (mk_abs_term (mk_var (mk_name [] "x") ty) mk_true_term) (mk_arrow_type ty mk_bool_type))) p in
    let refl = mk_refl p in
    beta_equal (mk_appThm (mk_subst mk_axiom_forall_ env_type env_var) refl) tl tr


  let mk_rule_intro_forall name ty te thm =
    let var = mk_var name ty in
    let true_thm = mk_axiom_true in
    let absThm = mk_absThm var (mk_deductAntiSym thm true_thm) in
    let lambda = mk_abs_term var te in
    let forall_thm = mk_axiom_forall lambda ty in
    let sym = mk_sym forall_thm in
    mk_eqMp absThm sym


  let mk_rule_elim_forall thm lambda ty t =
    let tforall = mk_axiom_forall lambda ty in
    let eqMp = mk_eqMp thm tforall in
    let refl = mk_refl t in
    let appThm = mk_appThm eqMp refl in
    let te = mk_app_term (mk_abs_term (mk_var (mk_name [] "v") ty) mk_true_term) t in
    let beta = mk_betaConv te in
    let true_thm = mk_axiom_true in
    let eqMp = mk_eqMp true_thm (mk_sym (mk_trans appThm beta)) in
    let beta = mk_betaConv (mk_app_term lambda t) in
    mk_eqMp eqMp beta


  let proj thm bool left right =
    let binop_type = mk_arrow_type mk_bool_type (mk_arrow_type mk_bool_type mk_bool_type) in
    let x = mk_var (mk_name [] "freshx") mk_bool_type in
    let y = mk_var (mk_name [] "freshy") mk_bool_type in
    let pleft = mk_abs_term x (mk_abs_term y (mk_var_term x)) in
    let pright = mk_abs_term x (mk_abs_term y (mk_var_term y)) in
    let side = if bool then pright else pleft in
    let xory = if bool then mk_var_term y else mk_var_term x in
    let tand = mk_axiom_and left right in
    let eqMp = mk_eqMp thm tand in
    let refl = mk_refl side in
    let appThm = mk_appThm eqMp refl in
    let vf = mk_var (mk_name [] "f") binop_type in
    let f = mk_var_term vf in
    let tl = mk_app_term (mk_abs_term vf (mk_app_term (mk_app_term f left) right)) side in
    let tr = mk_app_term (mk_abs_term vf (mk_app_term (mk_app_term f mk_true_term) mk_true_term)) side in
    let beta1 = beta_equal appThm tl tr in
    let betaConv = mk_betaConv (mk_app_term (mk_abs_term x (mk_abs_term y xory)) left) in
    let refl = mk_refl right in
    let appThm = mk_appThm betaConv refl in
    let sym = mk_sym appThm in
    let trans = mk_trans sym beta1 in
    let betaConv = mk_betaConv (mk_app_term (mk_abs_term x (mk_abs_term y xory)) mk_true_term) in
    let refl = mk_refl mk_true_term in
    let appThm = mk_appThm betaConv refl in
    let trans = mk_trans trans appThm in
    let tl = mk_app_term (mk_abs_term y (if bool then xory else left)) right in
    let tr = mk_app_term (mk_abs_term y (if bool then xory else mk_true_term)) mk_true_term in
    let beta = beta_equal trans tl tr in
    let sym = mk_sym beta in
    let true_thm = mk_axiom_true in
    let thm = mk_eqMp true_thm sym in
    thm


  let proj_left thm left right =
    proj thm false left right

  let proj_right thm left right =
    proj thm true left right

  let mk_rule_intro_impl thm p q =
    let binop_type = mk_arrow_type mk_bool_type (mk_arrow_type mk_bool_type mk_bool_type) in
    let assume = mk_assume p in
    let thm_true = mk_axiom_true in
    let deduct = mk_deductAntiSym assume thm_true in
    let vf = mk_var (mk_name [] "freshf") binop_type in
    let f = mk_var_term vf in
    let refl = mk_refl f in
    let appThm = mk_appThm refl deduct in
    let deduct = mk_deductAntiSym thm thm_true in
    let appThm = mk_appThm appThm deduct in
    let absThm = mk_absThm vf appThm in
    let tand = mk_axiom_and p q in
    let sym = mk_sym tand in
    let eqMp = mk_eqMp absThm sym in
    let assume = mk_assume (mk_and_term p q) in
    let proj_left = proj_left assume p q in
    let deduct = mk_deductAntiSym eqMp proj_left in
    let timpl = mk_axiom_impl p q in
    let sym = mk_sym timpl in
    mk_eqMp deduct sym


  let mk_rule_elim_impl thmp thmimpl p q =
    let timpl = mk_axiom_impl p q in
    let assume = mk_assume (mk_impl_term p q) in
    let eqMp = mk_eqMp assume timpl in
    let sym = mk_sym eqMp in
    let assume = mk_assume p in
    let eqMp = mk_eqMp assume sym in
    let proj_right = proj_right eqMp p q in
    let proveHyp = mk_proveHyp proj_right thmp in
    let thm = mk_proveHyp proveHyp thmimpl in
    thm


  let mk_impl_equal eqp eqq p q p' q' =
    let assume = mk_assume p' in
    let sym = mk_sym eqp in
    let eqMp = mk_eqMp assume sym in
    let assume = mk_assume (mk_impl_term p q) in
    let impl_elim = mk_rule_elim_impl eqMp assume p q in
    let eqMp = mk_eqMp impl_elim eqq in
    let intro_impl_left = mk_rule_intro_impl eqMp p' q' in

    let assume = mk_assume p in
    let eqMp = mk_eqMp assume eqp in
    let assume = mk_assume (mk_impl_term p' q') in
    let impl_elim = mk_rule_elim_impl eqMp assume p' q' in
    let sym = mk_sym eqq in
    let eqMp = mk_eqMp impl_elim sym in
    let intro_impl_right = mk_rule_intro_impl eqMp p q in
    mk_deductAntiSym intro_impl_right intro_impl_left


  let mk_forall_equal eq name left right ty =
    let lambda_l = mk_abs_term (mk_var name ty) left in
    let lambda_r = mk_abs_term (mk_var name ty) right in
    let assume = mk_assume (mk_forall_term lambda_l ty) in
    let tvar = mk_var_term (mk_var name ty) in
    let elim_forall = mk_rule_elim_forall assume lambda_l ty tvar in
    let eqMp = mk_eqMp elim_forall eq in
    let intro_forall_left = mk_rule_intro_forall name ty right eqMp in
    let assume = mk_assume (mk_forall_term lambda_r ty) in
    let tvar = mk_var_term (mk_var name ty) in
    let elim_forall = mk_rule_elim_forall assume lambda_r ty tvar in
    let sym = mk_sym eq in
    let eqMp = mk_eqMp elim_forall sym in
    let intro_forall_right = mk_rule_intro_forall name ty left eqMp in
    let deduct = mk_deductAntiSym intro_forall_left intro_forall_right in
    mk_sym deduct

  let mk_equal_equal l r l' r' ll' rr' ty =
    let sym = mk_sym ll' in
    let assume = mk_assume (mk_equal_term l r ty) in
    let trans = mk_trans sym assume in
    let l'r' = mk_trans trans rr' in

    let assume = mk_assume (mk_equal_term l' r' ty) in
    let trans = mk_trans ll' assume in
    let sym = mk_sym rr' in
    let lr = mk_trans trans sym in
    mk_deductAntiSym  lr l'r'

end
