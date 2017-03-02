open Basic
open Format
open Term

(* Miller's patterns *)
type pattern =
  | Var         of loc*ident*int*pattern list (* Y x1 ... xn *)
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term

let rec pp_pattern out pattern =
  match pattern with
  | Var (_, x, n, []) -> fprintf out "%a[%i]" pp_ident x n
  | Var (_, x, n, lst) -> fprintf out "%a[%i] %a" pp_ident x n (pp_list " " pp_pattern_wp) lst
  | Pattern (_, m, v,[]) -> fprintf out "%a.%a" pp_ident m pp_ident v
  | Pattern (_, m, v, pats) ->
    fprintf out "%a.%a %a" pp_ident m pp_ident v (pp_list " " pp_pattern_wp) pats
  | Lambda (_, x, p) -> fprintf out "%a => %a" pp_ident x pp_pattern p
  | Brackets t -> fprintf out "{ %a }" pp_term t

and pp_pattern_wp out pattern =
  match pattern with
  | Var (_, _, _, _::_) | Pattern _ | Lambda _ as p -> fprintf out "(%a)" pp_pattern p
  | p -> pp_pattern out p

type linear_pattern =
  | LJoker
  | LVar         of ident * int * int list
  | LLambda      of ident * linear_pattern
  | LPattern     of ident * ident * linear_pattern array
  | LBoundVar    of ident * int * linear_pattern array

let rec pp_linear_pattern fmt linear_pattern =
  match linear_pattern with
  | LJoker -> fprintf fmt "_"
  | LVar (x, n, []) -> fprintf fmt "%a[%i]" pp_ident x n
  | LVar (x, n, lst) ->
    fprintf fmt "%a[%i] %a" pp_ident x n (pp_list " " pp_print_int) lst
  | LPattern (m, v, pats) when Array.length pats = 0 -> fprintf fmt "%a.%a" pp_ident m pp_ident v
  | LPattern (m, v, pats) ->
    fprintf fmt "%a.%a %a" pp_ident m pp_ident v
      (pp_list " " pp_linear_pattern_wp) (Array.to_list pats)
  | LLambda (x, p) -> fprintf fmt "%a => %a" pp_ident x pp_linear_pattern p
  | LBoundVar(x, n, pats) when Array.length pats = 0 -> fprintf fmt "%a[%i]" pp_ident x n
  | LBoundVar(x,n, pats) ->
    fprintf fmt "%a[%i] %a" pp_ident x n (pp_list " " pp_linear_pattern_wp) (Array.to_list pats)

and pp_linear_pattern_wp fmt linear_pattern =
  match linear_pattern with
  | LVar(_, _, _::_) | LPattern _ | LLambda _ as p -> fprintf fmt "(%a)" pp_linear_pattern p
  | _ -> pp_linear_pattern fmt linear_pattern

let get_loc_pat = function
  | Var (l,_,_,_) | Pattern (l,_,_,_)
  | Lambda (l,_,_) -> l
  | Brackets t -> get_loc t


type untyped_context = ( loc * ident ) list

let pp_untyped_context fmt ctx =
  pp_list ", " (fun out (_,x) ->
      fprintf fmt "%a" pp_ident x) fmt (List.rev ctx)

type typed_context = ( loc * ident * term ) list

let pp_typed_context fmt ctx =
  pp_list ".\n" (fun fmtt (_,x,ty) ->
                   fprintf fmt "%a: %a" pp_ident x pp_term ty )
    fmt (List.rev ctx)

type 'a rule = 'a * pattern * term

type untyped_rule = untyped_context * pattern * term

type typed_rule = typed_context rule

let pp_untyped_rule fmt (ctx,pat,te) =
  fprintf fmt "[%a] %a --> %a"
    pp_untyped_context ctx
    pp_pattern pat
    pp_term te

let pp_typed_rule out (ctx,pat,te) =
    fprintf out "[%a] %a --> %a"
      pp_typed_context ctx
      pp_pattern pat
      pp_term te

type constr =
  | Linearity of int * int
  | Bracket of int * term

type rule_infos = {
  l : loc;
  ctx : typed_context;
  md : ident;
  id : ident;
  args : pattern list;
  rhs : term;
  esize : int;
  l_args : linear_pattern array;
  constraints : constr list;
}

(* FIXME: do not print all the informations because it is used in utils/errors *)
let pp_rule_infos out r =
  pp_typed_rule out (r.ctx,Pattern(r.l,r.md,r.id,r.args),r.rhs)


let pattern_to_term p =
  let rec aux k = function
    | Brackets t -> t
    | Pattern (l,m,v,[]) -> mk_Const l m v
    | Var (l,x,n,[]) -> mk_DB l x n
    | Pattern (l,m,v,a::args) ->
        mk_App (mk_Const l m v) (aux k a) (List.map (aux k) args)
    | Var (l,x,n,a::args) ->
        mk_App (mk_DB l x n) (aux k a) (List.map (aux k) args)
    | Lambda (l,x,pat) -> mk_Lam l x None (aux (k+1) pat)
  in
    aux 0 p
