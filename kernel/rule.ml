open Basic
open Format
open Term

type pattern =
  | Var      of loc * ident * int * pattern list (* Y x1 ... xn *)
  | Pattern  of loc * name * pattern list
  | Lambda   of loc * ident * pattern
  | Brackets of term

type rule_name = Beta | Delta of name | Gamma of bool * name

let rule_name_eq : rule_name -> rule_name -> bool = fun n1 n2 ->
  match n1,n2 with
  | Delta x    , Delta y       -> name_eq x y
  | Gamma (b1,x), Gamma (b2,y) -> b1=b2 && name_eq x y
  | _,_                        -> false

type 'a rule =
  {
    name: rule_name;
    ctx: 'a context;
    lhs: term;
    rhs:term
  }

type untyped_rule         = term option rule
type typed_rule           = term        rule


exception Not_a_pattern

let rec pattern_of_term t =
  let open Term in
  match t with
  | Kind | Type _ | Pi _ -> raise Not_a_pattern
  | Lam (lc, x, _, te) -> Lambda (lc, x, pattern_of_term te)
  | App (Const (lc, name), a, args) ->
      Pattern (lc, name, List.map pattern_of_term (a :: args))
  | App (DB (lc, x, n), a, args) ->
      Var (lc, x, n, List.map pattern_of_term (a :: args))
  | Const (lc, name) -> Pattern (lc, name, [])
  | DB (lc, x, n) -> Var (lc, x, n, [])
  | _ -> raise Not_a_pattern

let rec pp_pattern out pattern =
  match pattern with
  | Var (_, x, n, []) -> fprintf out "%a[%i]" pp_ident x n
  | Var (_, x, n, lst) -> fprintf out "%a[%i] %a" pp_ident x n (pp_list " " pp_pattern_wp) lst
  | Pattern (_, n,[]) -> fprintf out "%a" pp_name n
  | Pattern (_, n, pats) ->
    fprintf out "%a %a" pp_name n (pp_list " " pp_pattern_wp) pats
  | Lambda (_, x, p) -> fprintf out "%a => %a" pp_ident x pp_pattern p
  | Brackets t -> fprintf out "{ %a }" pp_term t


and pp_pattern_wp out pattern =
  match pattern with
  | Var (_, _, _, _::_) | Pattern _ | Lambda _ as p -> fprintf out "(%a)" pp_pattern p
  | p -> pp_pattern out p

let get_loc_pat = function
  | Var (l,_,_,_) | Pattern (l,_,_)
  | Lambda (l,_,_) -> l
  | Brackets t -> get_loc t

let get_loc_rule r = Term.get_loc r.lhs


let pp_rule_name fmt = function
  | Beta            -> fprintf fmt "Beta"
  | Delta(n)        -> fprintf fmt "Delta: %a"           pp_name n
  | Gamma(true , n) -> fprintf fmt "Gamma: %a"           pp_name n
  | Gamma(false, n) -> fprintf fmt "Gamma (default): %a" pp_name n

let pp_rule pp_ctxt fmt (rule:'a rule) =
  fprintf fmt " {%a} [%a] %a --> %a"
    pp_rule_name rule.name
    pp_ctxt rule.ctx
    pp_term rule.lhs
    pp_term rule.rhs

let pp_untyped_rule fmt = pp_rule pp_untyped_context fmt
let pp_typed_rule       = pp_rule pp_typed_context
let pp_part_typed_rule  = pp_rule pp_part_typed_context

let pattern_to_term p =
  let rec aux k = function
    | Brackets t         -> t
    | Pattern (l,n,args) -> mk_App2 (mk_Const l n) (List.map (aux k) args)
    | Var (l,x,n,args)   -> mk_App2 (mk_DB  l x n) (List.map (aux k) args)
    | Lambda (l,x,pat)   -> mk_Lam l x None (aux (k+1) pat)
  in
  aux 0 p
