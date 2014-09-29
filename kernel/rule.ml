open Basics
open Term

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list
  | PLambda     of loc*ident*prepattern
  | PJoker      of loc

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = loc * pdecl list * ident * prepattern list * preterm

type pattern =
  | MatchingVar of loc*ident*int*(loc*ident*int) list
  | BoundVar    of loc*ident*int*pattern list
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term
  | Joker       of loc

let get_loc_pat = function
  | MatchingVar (l,_,_,_) | BoundVar (l,_,_,_) | Pattern (l,_,_,_)
  | Lambda (l,_,_) | Joker l -> l
  | Brackets t -> get_loc t

type top = ident*pattern array

type rule = {
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
    | BoundVar (l,x,n,[]) | MatchingVar (l,x,n,[]) -> mk_DB l x n
    | Pattern (l,m,v,a::args) ->
        mk_App (mk_Const l m v) (aux k a) (List.map (aux k) args)
    | BoundVar (l,x,n,a::args) ->
        mk_App (mk_DB l x n) (aux k a) (List.map (aux k) args)
    | MatchingVar (l,x,n,(l2,x2,n2)::args) ->
        mk_App (mk_DB l x n) (aux k (BoundVar(l2,x2,n2,[])))
          (List.map (fun (l,x,n) -> aux k (BoundVar(l,x,n,[]))) args)
    | Lambda (l,x,pat) -> mk_Lam l x None (aux (k+1) pat)
  in
    aux 0 p
