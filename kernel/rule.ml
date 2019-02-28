open Basic
open Format
open Term

type pattern =
  | Var      of loc * ident * int * pattern list (* Y x1 ... xn *)
  | Pattern  of loc * name * pattern list
  | Lambda   of loc * ident * pattern
  | Brackets of term

type wf_pattern =
  | LJoker
  | LVar      of ident * int * int list
  | LLambda   of ident * wf_pattern
  | LPattern  of name * wf_pattern array
  | LBoundVar of ident * int * wf_pattern array

type rule_name = Beta | Delta of name | Gamma of bool * name

let rule_name_eq : rule_name -> rule_name -> bool = fun n1 n2 ->
  match n1,n2 with
  | Delta x    , Delta y       -> name_eq x y
  | Gamma (b1,x), Gamma (b2,y) -> b1=b2 && name_eq x y
  | _,_                        -> false

type 'a rule =
  {
    name: rule_name;
    ctx: 'a;
    pat: pattern;
    rhs:term
  }

type untyped_rule = untyped_context rule

type typed_rule = typed_context rule

(* TODO : maybe replace constr by Linearity | Bracket and constr list by a constr Map.t *)
type constr =
  | Linearity of int * int
  | Bracket   of int * term

type rule_infos = {
  l           : loc;
  name        : rule_name;
  cst         : name;
  args        : pattern list;
  rhs         : term;
  esize       : int;
  pats        : wf_pattern array;
  arity       : int array;
  constraints : constr list;
}

let infer_rule_context ri =
  let res = Array.make ri.esize (mk_ident "_") in
  let rec aux k = function
    | LJoker -> ()
    | LVar (name,n,args) -> res.(n-k) <- name
    | LLambda (_,body) -> aux (k+1) body
    | LPattern  (_  ,args) -> Array.iter (aux k) args
    | LBoundVar (_,_,args) -> Array.iter (aux k) args
  in
  Array.iter (aux 0) ri.pats;
  List.map (fun i -> (dloc, i)) (Array.to_list res)


let pattern_of_rule_infos r = Pattern (r.l,r.cst,r.args)

type rule_error =
  | BoundVariableExpected          of pattern
  | DistinctBoundVariablesExpected of loc * ident
  | VariableBoundOutsideTheGuard   of term
  | UnboundVariable                of loc * ident * pattern
  (* FIXME : this exception seems never to be raised *)
  | AVariableIsNotAPattern         of loc * ident
  | NonLinearNonEqArguments        of loc * ident
  (* FIXME: the reason for this exception should be formalized on paper ! *)

exception RuleError of rule_error

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
  | LPattern (n, pats) -> fprintf fmt "%a %a" pp_name n
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

let get_loc_rule r = get_loc_pat r.pat

let pp_typed_ident fmt (id,ty) = Format.fprintf fmt "%a:%a" pp_ident id pp_term ty

let pp_context pp_i fmt l = fprintf fmt "[%a]" (pp_list ", " pp_i) (List.rev l)

let pp_untyped_context fmt ctx =
  pp_context pp_ident       fmt (List.map snd                      ctx)
let pp_typed_context   fmt ctx =
  pp_context pp_typed_ident fmt (List.map (fun (_,a,ty) -> (a,ty)) ctx)

let pp_rule_name fmt = function
  | Beta            -> fprintf fmt "Beta"
  | Delta(n)        -> fprintf fmt "Delta: %a"           pp_name n
  | Gamma(true , n) -> fprintf fmt "Gamma: %a"           pp_name n
  | Gamma(false, n) -> fprintf fmt "Gamma (default): %a" pp_name n

let pp_rule pp_ctxt fmt (rule:'a rule) =
  fprintf fmt " {%a} [%a] %a --> %a"
    pp_rule_name rule.name
    pp_ctxt rule.ctx
    pp_pattern rule.pat
    pp_term rule.rhs

let pp_untyped_rule = pp_rule pp_untyped_context
let pp_typed_rule   = pp_rule pp_typed_context

(* FIXME: do not print all the informations because it is used in utils/errors *)
let pp_rule_infos out r =
  pp_untyped_rule out
    { name = r.name;
      ctx = infer_rule_context r;
      pat = pattern_of_rule_infos r;
      rhs = r.rhs
    }

type pattern_info =
  {
    constraints  : constr list;
    context_size : int;
    arity        : int array;
  }

(* ************************************************************************** *)


(* ************************************************************************** *)

let bracket_ident = mk_ident "{_}"  (* FIXME: can this be replaced by dmark? *)

let rec all_distinct = function
  | [] -> true
  | hd::tl -> if List.mem hd tl then false else all_distinct tl

module IntHashtbl =
  Hashtbl.Make(struct
    type t = int
    let equal i j = i=j
    let hash i = i land max_int
  end
  )

(* TODO : cut this function in smaller ones *)
(** [check_patterns size pats] checks that the given pattern is a well formed
Miller pattern in a context of size [size] and linearizes it.

More precisely:
- Context variables are exclusively applied to distinct locally bound variables
- Occurences of each context variable are all applied to the same number of arguments

Returns the representation of the corresponding linear well formed pattern
together with extracted pattern information:
- Convertibility constraints from non-linearity and brackets
- Size of generated context
- Arity infered for all context variables
*)
let check_patterns (esize:int) (pats:pattern list) : wf_pattern list * pattern_info =
  let constraints  = ref [] in
  let context_size = ref esize in
  let arity = IntHashtbl.create 10 in
  let fresh_var ar = (* DB indice for a fresh context variable with given arity *)
    IntHashtbl.add arity !context_size ar;
    incr context_size;
    !context_size - 1 in
  let extract_db k pat =
    match pat with
    | Var (_,_,n,[]) when n<k -> n
    | p -> raise (RuleError (BoundVariableExpected p))
  in
  let rec aux (k:int) (pat:pattern) : wf_pattern =
    match pat with
    | Lambda (l,x,p) -> LLambda (x, aux (k+1) p)
    | Var (l,x,n,args) when n<k ->
      LBoundVar(x, n, Array.of_list (List.map (aux k) args))
    | Var (l,x,n,args) (* Context variable (n>=k)  *) ->
      (* Miller variables should only be applied to locally bound variables *)
      let args' = List.map (extract_db k) args in
      (* Miller variables should be applied to distinct variables *)
      if not (all_distinct args')
      then raise (RuleError (DistinctBoundVariablesExpected (l,x)));
      let nb_args' = List.length args' in
      if IntHashtbl.mem arity (n-k)
      then if nb_args' <> IntHashtbl.find arity (n-k)
        then raise (RuleError (NonLinearNonEqArguments(l,x)))
        else
          let nvar = fresh_var nb_args' in
          constraints := Linearity(nvar, n-k) :: !constraints;
          LVar(x, nvar + k, args')
      else
        let _ = IntHashtbl.add arity (n-k) nb_args' in
        LVar(x,n,args')
    | Brackets t ->
      let unshifted =
        try Subst.unshift k t
        with Subst.UnshiftExn -> raise (RuleError (VariableBoundOutsideTheGuard t))
        (* Note: A different exception is previously raised at rule type-checking for this. *)
      in
      let nvar = fresh_var 0 in
      constraints := Bracket (nvar, unshifted) :: !constraints;
      LVar(bracket_ident, nvar + k, [])
    | Pattern (_,n,args) -> LPattern(n, Array.of_list  (List.map (aux k) args))
  in
  let wf_pats = List.map (aux 0) pats in
  ( wf_pats
  , { context_size = !context_size;
      constraints = !constraints;
      arity = Array.init !context_size (fun i -> IntHashtbl.find arity i)
    } )

let check_brackets (rule:typed_rule) : unit =
  let ctxt = rule.ctx in
  let pats = rule.pat in
  ()

let to_rule_infos (r:'a context rule) : rule_infos =
  let esize = List.length r.ctx in
  let (l,cst,args) = match r.pat with
    | Pattern (l,cst,args) -> (l, cst, args)
    | Var (l,x,_,_) -> raise (RuleError (AVariableIsNotAPattern (l,x)))
    | Lambda _ | Brackets _ -> assert false (* already raised at the parsing level *)
  in
  let (pats2,infos) = check_patterns esize args in
  { l ;
    name = r.name ;
    cst ; args ; rhs = r.rhs ;
    esize = infos.context_size ;
    pats = Array.of_list pats2 ;
    arity = infos.arity ;
    constraints = infos.constraints
  }
