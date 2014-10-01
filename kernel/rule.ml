open Basics
open Term

type pattern =
  | Var         of loc*ident*int*pattern list
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term
  | Joker       of loc

let get_loc_pat = function
  | Var (l,_,_,_) | Pattern (l,_,_,_)
  | Lambda (l,_,_) | Joker l -> l
  | Brackets t -> get_loc t

type top = ident*pattern array

type rule = context * pattern * term
type frule = {
  l:loc; ctx:context; md:ident; id:ident; args:pattern list; rhs:term; }

type case =
  | CConst of int*ident*ident
  | CDB    of int*int
  | CLam

type abstract_pb = int (*c*) * int LList.t (*(k_i)_{i<=n}*)

type pre_context =
  | Syntactic of int LList.t
  | MillerPattern of abstract_pb LList.t

type dtree =
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of pre_context * (term*term) list * term * dtree option

let pattern_to_term p =
  let rec aux k = function
    | Joker _ -> assert false
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
