module B = Kernel.Basic
module L = Common.Logic
module O = Common.Oracle
module U = Common.Universes
module Z = Z3

type t         = Z.Expr.expr
type smt_model = Z.Model.model
type ctx       = Z.context

let logic = `Qfuf

(* Z3 type for universes *)
let sort ctx      = Z.Sort.mk_uninterpreted_s ctx "Sort"

(** [var_of_name name] returns a variable string for Z3. *)
let mk_name cst = B.string_of_mident (B.md cst) ^ (B.string_of_ident (B.id cst))

let mk_enum ctx i =
  Z.Expr.mk_const_s ctx ("Enum"^string_of_int i) (sort ctx)

let mk_var ctx s = Z.Expr.mk_const_s ctx s (sort ctx)

let mk_sinf ctx = Z.Expr.mk_const_s ctx ("Sinf") (sort ctx)

(** [mk_univ ctx u] construct a Z3 expression from a universe. *)
let mk_univ ctx = function
  | U.Sinf  -> mk_sinf ctx
  | U.Var x -> mk_var ctx (mk_name x)
  | U.Enum i -> mk_enum ctx i

let bool_sort ctx = Z.Boolean.mk_sort ctx

(** [mk_axiom s s'] construct the Z3 predicate associated to the Axiom Predicate *)
let mk_axiom ctx s s' =
  let sort = sort ctx in
  let bool_sort = bool_sort ctx in
  let axiom = Z.FuncDecl.mk_func_decl_s ctx "A" [sort;sort] bool_sort in
  Z.Expr.mk_app ctx axiom [s;s']

(** [mk_cumul s s'] construct the Z3 predicate associated to the Cumul Predicate *)
let mk_cumul ctx s s' =
  let sort = sort ctx in
  let bool_sort = bool_sort ctx in
  let cumul = Z.FuncDecl.mk_func_decl_s ctx "C" [sort;sort] bool_sort in
  Z.Expr.mk_app ctx cumul [s;s']

(** [mk_rule s s' s''] construct the Z3 predicate associated to the Rule Predicate *)
let mk_rule ctx s s' s'' =
  let sort = sort ctx in
  let bool_sort = bool_sort ctx in
  let cumul = Z.FuncDecl.mk_func_decl_s ctx "R" [sort;sort;sort] bool_sort in
  Z.Expr.mk_app ctx cumul [s;s';s'']

(** [register_vars vars i] give bound for each variable [var] between [0] and [i] *)
let mk_bounds ctx var i =
  let univs = O.enumerate i in
  let or_eqs = List.map (fun u -> Z.Boolean.mk_eq ctx (mk_var ctx var) (mk_univ ctx u)) univs in
  Z.Boolean.mk_or ctx or_eqs

(** [solution_of_var univs model var] looks for the concrete universe associated to [var]
    in the [model]. Such universe satisfy that model(univ) = model(var). *)
let solution_of_var ctx i model var =
  let univs = O.enumerate i in
  let exception Found of U.univ in
  let find_univ e u  =
    match Z.Model.get_const_interp_e model (mk_univ ctx u) with
    | None -> assert false
    | Some u' ->
      if e = u' then raise (Found u) else ()
  in
  match Z.Model.get_const_interp_e model (mk_var ctx var) with
  | None -> assert false
  | Some e ->
    try
      List.iter (find_univ e) univs;
      failwith "Variable was not found in the model, this error should be reported"
    with Found(u) -> u
