open Basic
open Term

(* Miller's patterns *)
type pattern =
  | Var         of loc*ident*int*pattern list (* Y x1 ... xn *)
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term

type untyped_context = ( loc * ident ) list

type typed_context = ( loc * ident * term ) list

let get_loc_pat = function
  | Var (l,_,_,_) | Pattern (l,_,_,_)
  | Lambda (l,_,_) -> l
  | Brackets t -> get_loc t

type 'a rule = 'a * pattern * term

type typed_rule = typed_context rule
type untyped_rule = untyped_context * pattern * term

type linear_pattern =
  | Joker2
  | Var2         of ident * int * int list
  | Lambda2      of ident * linear_pattern
  | Pattern2     of ident * ident * linear_pattern array
  | BoundVar2    of ident * int * linear_pattern array

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

type case =
  | CConst of int*ident*ident
  | CDB    of int*int
  | CLam

type abstract_pb = { position2:int (*c*) ; dbs:int LList.t (*(k_i)_{i<=n}*) ; depth2:int }
type pos = { position:int; depth:int }

type pre_context =
  | Syntactic of pos LList.t
  | MillerPattern of abstract_pb LList.t

type dtree =
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of pre_context * constr list * term * dtree option

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
