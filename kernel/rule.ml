open Basic
open Format
open Term

(* Miller's patterns *)
type pattern =
  | Var         of loc*ident*int*pattern list (* Y x1 ... xn *)
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term

type linear_pattern =
  | LJoker
  | LVar         of ident * int * int list
  | LLambda      of ident * linear_pattern
  | LPattern     of ident * ident * linear_pattern array
  | LBoundVar    of ident * int * linear_pattern array

type untyped_context = ( loc * ident ) list

type typed_context = ( loc * ident * term ) list

type 'a rule = 'a * pattern * term

type untyped_rule = untyped_context * pattern * term

type typed_rule = typed_context rule

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


type rule_error =
  | BoundVariableExpected of pattern
  | DistinctBoundVariablesExpected of loc * ident
  | VariableBoundOutsideTheGuard of term

exception DtreeExn of rule_error

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

let pp_untyped_context fmt ctx =
  pp_list ", " (fun out (_,x) ->
      fprintf fmt "%a" pp_ident x) fmt (List.rev ctx)


let pp_typed_context fmt ctx =
  pp_list ".\n" (fun fmtt (_,x,ty) ->
                   fprintf fmt "%a: %a" pp_ident x pp_term ty )
    fmt (List.rev ctx)

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


module IntSet = Set.Make(struct type t=int let compare=(-) end)

type lin_ty = { cstr:constr list; fvar:int ; seen:IntSet.t }

(* ************************************************************************** *)

let fold_map (f:'b->'a->('c*'b)) (b0:'b) (alst:'a list) : ('c list*'b) =
  let (clst,b2) =
    List.fold_left (fun (accu,b1) a -> let (c,b2) = f b1 a in (c::accu,b2))
      ([],b0) alst in
    ( List.rev clst , b2 )

(* FIXME : dangerous to put this as it is. *)
let br = hstring "{_}"

let rec all_distinct = function
  | [] -> true
  | hd::tl -> if List.mem hd tl then false else all_distinct tl


let extract_db k = function
  | Var (_,_,n,[]) when n<k -> n
  | p -> raise (DtreeExn (BoundVariableExpected p))

(* ************************************************************************** *)

let allow_non_linear = ref false

(* This function extracts non-linearity and bracket constraints from a list
 * of patterns. *)
(* TODO : this should be inside Rule module *)
let linearize (esize:int) (lst:pattern list) : int * linear_pattern list * constr list =
  let rec aux k (s:lin_ty) = function
  | Lambda (l,x,p) ->
      let (p2,s2) = (aux (k+1) s p) in
        ( LLambda (x,p2) , s2 )
  | Var (l,x,n,args) when n<k ->
      let (args2,s2) = fold_map (aux k) s args in
        ( LBoundVar (x,n,Array.of_list args2) , s2 )
  | Var (l,x,n,args) (* n>=k *) ->
    let args2 = List.map (extract_db k) args in
    (* to keep a most general unifier, all arguments applied to an higher-order variable should be distincts.
       eg : (x => F x x) =?= (x => x) implies that F is either (x => y => x) or (x => y => y) *)
    if all_distinct args2 then
      begin
        if IntSet.mem (n-k) (s.seen) then
          ( LVar(x,s.fvar+k,args2),
            { s with fvar=(s.fvar+1);
                     cstr= (Linearity (s.fvar,n-k))::(s.cstr) ; } )
        else
          ( LVar(x,n,args2) , { s with seen=IntSet.add (n-k) s.seen; } )
      end
    else
      raise (DtreeExn (DistinctBoundVariablesExpected (l,x)))
    | Brackets t ->
      begin
        try
          ( LVar(br,s.fvar+k,[]),
            { s with fvar=(s.fvar+1);
                     cstr=(Bracket (s.fvar,Subst.unshift k t))::(s.cstr) ;} )
        with
        | Subst.UnshiftExn -> raise (DtreeExn (VariableBoundOutsideTheGuard t))
      end
    | Pattern (_,m,v,args) ->
        let (args2,s2) = (fold_map (aux k) s args) in
          ( LPattern(m,v,Array.of_list args2) , s2 )
  in
  let (lst,r) = fold_map (aux 0) { fvar=esize; cstr=[]; seen=IntSet.empty; } lst in
    ( r.fvar , lst , r.cstr )
