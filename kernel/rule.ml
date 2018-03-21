open Basic
open Format
open Term

(* Miller's patterns *)
type pattern =
  | Var         of loc * ident * int * pattern list (* Y x1 ... xn *)
  | Pattern     of loc * name * pattern list
  | Lambda      of loc * ident * pattern
  | Brackets    of term

type wf_pattern =
  | LJoker
  | LVar         of ident * int * int list
  | LLambda      of ident * wf_pattern
  | LPattern     of name * wf_pattern array
  | LBoundVar    of ident * int * wf_pattern array

type rule_name = Delta of name | Gamma of bool * name

type 'a rule =
  {
    name: rule_name;
    ctx: 'a;
    pat: pattern;
    rhs:term
  }

type untyped_rule = untyped_context rule

type typed_rule = typed_context rule

(* TODO : may be replace constr by Linearity | Bracket and constr list by a constr Map.t *)
type constr =
  | Linearity of int * int
  | Bracket of int * term

type rule_infos = {
  l : loc;
  name : rule_name ;
  ctx : typed_context;
  cst : name;
  args : pattern list;
  rhs : term;
  esize : int;
  pats : wf_pattern array;
  constraints : constr list;
}

let pattern_of_rule_infos r = Pattern (r.l,r.cst,r.args)

type rule_error =
  | BoundVariableExpected of pattern
  | DistinctBoundVariablesExpected of loc * ident
  | VariableBoundOutsideTheGuard of term
  | UnboundVariable of loc * ident * pattern (* FIXME: this exception seems never to be raised *)
  | AVariableIsNotAPattern of loc * ident
  | NonLinearRule of typed_rule
  | NotEnoughArguments of loc * ident * int * int * int
  | NonLinearNonEqArguments of loc * ident (* FIXME: this necessary condition should be formalalized on paper *)

exception RuleExn of rule_error

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

let rec pp_wf_pattern fmt wf_pattern =
  match wf_pattern with
  | LJoker -> fprintf fmt "_"
  | LVar (x, n, []) -> fprintf fmt "%a[%i]" pp_ident x n
  | LVar (x, n, lst) ->
    fprintf fmt "%a[%i] %a" pp_ident x n (pp_list " " pp_print_int) lst
  | LPattern (n, pats) when Array.length pats = 0 -> fprintf fmt "%a" pp_name n
  | LPattern (n, pats) ->
    fprintf fmt "%a %a" pp_name n
      (pp_list " " pp_wf_pattern_wp) (Array.to_list pats)
  | LLambda (x, p) -> fprintf fmt "%a => %a" pp_ident x pp_wf_pattern p
  | LBoundVar(x, n, pats) when Array.length pats = 0 -> fprintf fmt "%a[%i]" pp_ident x n
  | LBoundVar(x,n, pats) ->
    fprintf fmt "%a[%i] %a" pp_ident x n (pp_list " " pp_wf_pattern_wp) (Array.to_list pats)

and pp_wf_pattern_wp fmt wf_pattern =
  match wf_pattern with
  | LVar(_, _, _::_) | LPattern _ | LLambda _ as p -> fprintf fmt "(%a)" pp_wf_pattern p
  | _ -> pp_wf_pattern fmt wf_pattern

let get_loc_pat = function
  | Var (l,_,_,_) | Pattern (l,_,_)
  | Lambda (l,_,_) -> l
  | Brackets t -> get_loc t

let pp_untyped_context fmt ctx =
  pp_list ", " (fun out (_,x) ->
      fprintf fmt "%a" pp_ident x) fmt (List.rev ctx)


let pp_typed_context fmt ctx =
  pp_list ".\n" (fun fmtt (_,x,ty) ->
                   fprintf fmt "%a: %a" pp_ident x pp_term ty )
    fmt (List.rev ctx)

let pp_rule_name fmt rule_name =
  let sort,n =
    match rule_name with
    | Delta(n) -> "Delta", n
    | Gamma(b,n) ->
      if b then
        "Gamma", n
      else
        "Gamma (default)", n
  in
  fprintf fmt "%s: %a" sort pp_name n

(* FIXME: factorize this function with the follozing one *)
let pp_untyped_rule fmt (rule:untyped_rule) =
  fprintf fmt " {%a} [%a] %a --> %a"
    pp_untyped_context rule.ctx
    pp_pattern rule.pat
    pp_term rule.rhs
    pp_rule_name rule.name

let pp_typed_rule fmt (rule:typed_rule) =
  fprintf fmt " {%a} [%a] %a --> %a"
    pp_typed_context rule.ctx
    pp_pattern rule.pat
    pp_term rule.rhs
    pp_rule_name rule.name

(* FIXME: do not print all the informations because it is used in utils/errors *)
let pp_rule_infos out r =
  let rule = { name = r.name; ctx = r.ctx;
               pat = pattern_of_rule_infos r;
               rhs = r.rhs } in
  pp_typed_rule out rule

let pattern_to_term p =
  let rec aux k = function
    | Brackets t -> t
    | Pattern (l,n,[]) -> mk_Const l n
    | Var (l,x,n,[]) -> mk_DB l x n
    | Pattern (l,n,a::args) ->
        mk_App (mk_Const l n) (aux k a) (List.map (aux k) args)
    | Var (l,x,n,a::args) ->
        mk_App (mk_DB l x n) (aux k a) (List.map (aux k) args)
    | Lambda (l,x,pat) -> mk_Lam l x None (aux (k+1) pat)
  in
  aux 0 p

module IntMap = Map.Make(struct type t=int let compare=(-) end)

type pattern_info =
  {
    constraints  : constr list;
    context_size : int;
    arity        : int IntMap.t
  }

(* ************************************************************************** *)


(* ************************************************************************** *)

let allow_non_linear = ref false

let bracket_ident = mk_ident "{_}"  (* FIXME: can this be replaced by dmark? *)

let rec all_distinct = function
  | [] -> true
  | hd::tl -> if List.mem hd tl then false else all_distinct tl


(* TODO : cut this function in smaller ones *)
(** [check_patterns size ps] checks that the given patterns are a well formed
Miller pattern in a context of size [size].

More precisely:
- Context variables are exclusively applied to distinct locally bound variables
- Occurences of such variable are all applied to the same number of arguments

Returns the corresponding well formed patterns as well as pattern information:
- Convertibility constraints from non-linearity and brackets
- Size of generated context
- Arity infered for all context variables
*)
let check_patterns (esize:int) (pats:pattern list) : wf_pattern list * pattern_info =
  let extract_db k pat =
    match pat with
    | Var (_,_,n,[]) when n<k -> n
    | p -> raise (RuleExn (BoundVariableExpected p))
  in
  let rec aux (k:int) (s:pattern_info) (pat:pattern) : wf_pattern * pattern_info =
    match pat with
    | Lambda (l,x,p) ->
      let (p2,s2) = (aux (k+1) s p) in
      ( LLambda (x,p2) , s2 )
    | Var (l,x,n,args) when n<k ->
      let (args2,s2) = fold_map (aux k) s args in
      ( LBoundVar (x,n,Array.of_list args2) , s2 )
    | Var (l,x,n,args) (* n>=k *) ->
      (* Miller variables should only be applied to locally bound variables *)
      let args' = List.map (extract_db k) args in
      (* Miller variables should be applied to distinct variables *)
      if not (all_distinct args')
      then raise (RuleExn (DistinctBoundVariablesExpected (l,x)));
      let nb_args' = List.length args' in
      if IntMap.mem (n-k) s.arity
      then
        if nb_args' <> IntMap.find (n-k) s.arity
        then raise (RuleExn (NonLinearNonEqArguments(l,x)))
        else
          let fvar = s.context_size in
          ( LVar(x,fvar+k,args') (* Well formed pattern *)
          , {s with
             context_size=(fvar+1);
             constraints=(Linearity (fvar, n-k))::(s.constraints)}
          )
      else
        let arity' = IntMap.add (n-k) nb_args' s.arity in
        ( LVar(x,n,args') , { s with arity = arity' } )
    | Brackets t ->
      let fvar = s.context_size in
      let wf_pat = LVar(bracket_ident,fvar+k,[]) in
      let cstr =
        try Bracket (fvar, Subst.unshift k t)
        with Subst.UnshiftExn -> raise (RuleExn (VariableBoundOutsideTheGuard t))
      in
      let constraints = { s with
                         context_size = s.context_size + 1;
                         constraints  = cstr :: s.constraints} in
      (wf_pat, constraints)
    | Pattern (_,n,args) ->
      let (args2,s2) = (fold_map (aux k) s args) in
      ( LPattern(n, Array.of_list args2), s2 )
  in
  let pat_infos = {context_size = esize; constraints = []; arity = IntMap.empty; } in
  fold_map (aux 0) pat_infos pats

(* Checks that the lhs variables are applied to enough arguments *)
let check_nb_args (arity:int IntMap.t) (rhs:term) : unit =
  let check l id n k nargs =
    let expected_args = IntMap.find (n-k) arity in
    if nargs < expected_args
    then raise (RuleExn (NotEnoughArguments (l,id,n,nargs,expected_args))) in
  let rec aux k = function
    | Kind | Type _ | Const _ -> ()
    | DB (l,id,n) ->
      if n >= k then check l id n k 0
    | App(DB(l,id,n),a1,args) when n>=k ->
      check l id n k (List.length args + 1);
      List.iter (aux k) (a1::args)
    | App (f,a1,args) -> List.iter (aux k) (f::a1::args)
    | Lam (_,_,None,b) -> aux (k+1) b
    | Lam (_,_,Some a,b) | Pi (_,_,a,b) -> (aux k a;  aux (k+1) b)
  in
  aux 0 rhs

let to_rule_infos (r:typed_rule) : (rule_infos,rule_error) error =
  let is_linear = List.for_all (function Linearity _ -> false | _ -> true) in
  try
    begin
      let esize = List.length r.ctx in
      let (l,cst,args) = match r.pat with
        | Pattern (l,cst,args) -> (l, cst, args)
        | Var (l,x,_,_) -> raise (RuleExn (AVariableIsNotAPattern (l,x)))
        | Lambda _ | Brackets _ -> assert false (* already raised at the parsing level *)
      in
      let (pats2,infos) = check_patterns  esize args in
      
      (* Checking that Miller variable are correctly applied in lhs *)
      check_nb_args infos.arity r.rhs;
      
      (* Checking if pattern has linearity constraints *)
      if not (is_linear infos.constraints)
      then
        if !allow_non_linear
        then debug 1 "Non-linear Rewrite Rule detected"
        else raise (RuleExn (NonLinearRule r));
      
      OK { l ; name = r.name ; ctx = r.ctx ; cst ; args ; rhs = r.rhs ;
           esize = infos.context_size ;
           pats = Array.of_list pats2 ;
           constraints = infos.constraints ; }
    end
  with
    RuleExn e -> Err e
