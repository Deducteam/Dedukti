
open Types

type 'a substitution = (int*'a) list

module type UTerm =
sig
  type term
  val subst: term substitution -> term -> term
  val occurs_check: int -> term -> bool
  val decompose: (int*term) list -> (term*term) list -> ((int*term) list) option2
end

module Make = functor (T: UTerm) ->
struct
  type state = (T.term*T.term) list * (int*T.term) list * T.term substitution

  let rec safe_assoc v = function
  | []                  -> None
  | (x,t)::_ when x=v   -> Some t
  | _::tl               -> safe_assoc v tl

  let rec unify0 : state -> (T.term substitution) option2 = function
    | ( [], [] , s )            -> Some2 s
    | ( [], (v,te0)::lst , s )  ->
        ( let te = T.subst s te0 in
            if T.occurs_check v te then None2
            else
              match safe_assoc v s with
                | Some te'      -> unify0 ( [(te,te')] , lst , s )
                | None      ->
                    let s' = List.map (fun (z,t) -> (z,T.subst [(v,te)] t)) s in
                      unify0 ( [] , lst , (v,te)::s' )
        )
    | ( a , b , s )      ->
        ( match T.decompose b a with
            | Some2 b'  -> unify0 ( [] , b' , s )
            | opt2      -> opt2
        )

  let unify lst = unify0 ( lst , [] , [] )
end

(* Partial Higher Order Unification *)

module TU : UTerm with type term = Types.term =
struct
  type term = Types.term
  let subst = Subst.subst_meta

  let rec add_lst a l1 l2 =
    match l1, l2 with
      | [], []                  -> Some a
      | a1::l1', a2::l2'        -> add_lst ((a1,a2)::a) l1' l2'
      | _,_                     -> None

  let is_neutral = function
    | []                                        -> assert false
    | Type::_ | Kind::_ | (Pi(_,_,_))::_        -> true
    | (DB(_,_))::_ | (Lam(_,_,_))::_            -> false
    | (Meta _)::_ | (App _)::_                  -> assert false
    | (Const (m,v))::_                          ->
        ( match Env.get_global_symbol dloc m v with
            | Env.Decl(_,None)  -> true
            | _                 -> false )

  let rec decompose b = function
    | []                -> Some2 b
    | (t1,t2)::a        ->
        begin
          match Reduction.bounded_whnf 500 t1,
                Reduction.bounded_whnf 500 t2 with
            | Some t1', Some t2'        ->
                begin
                  match t1', t2' with
                    | Meta n, t  | t, Meta n            -> decompose ((n,t)::b) a
                    | Kind, Kind | Type, Type           -> decompose b a
                    | DB(_,n1), DB(_,n2) when n1=n2     -> decompose b a
                    | Const(m1,v1), Const(m2,v2) when
                        (ident_eq v1 v2 && ident_eq m1 m2) -> decompose b a
                    | Pi(_,a1,b1), Pi(_,a2,b2)
                    | Lam(_,a1,b1), Lam(_,a2,b2)        ->
                        decompose b ((a1,a2)::(b1,b2)::a)
                    | App lst1 , t | t , App lst1       ->
                        if is_neutral lst1 then
                          ( match t with
                              | App lst2 ->
                                  if is_neutral lst2 then
                                    ( match add_lst a lst1 lst2 with
                                        | Some a'       -> decompose b a'
                                        | None          -> None2 )
                                  else DontKnow
                              | _        -> None2 )
                        else DontKnow
                    | _ , _                             -> None2
                end
            | _,_                       -> DontKnow
        end

  let rec occurs_check n = function
    | Meta k                      -> n=k
    | Pi (_,a,b) | Lam (_,a,b)    -> occurs_check n a || occurs_check n b
    | App lst                     -> List.exists (occurs_check n) lst
    | _                           -> false
end

module TUnification = Make(TU)

let unify_t = TUnification.unify

(* Pattern Unification *)

module PU : UTerm with type term = Types.pattern =
struct
  type term = Types.pattern
  let subst = Subst.subst_pattern

  let rec occurs_check n = function
    | Var (_,k)           -> n=k
    | Pattern(_,_,args) -> aux n args 0
  and aux n args i =
    if i < Array.length args then
      if occurs_check n args.(i) then true
      else aux n args (i+1)
    else false

  let add_to_list lst0 arr1 arr2 =
    (*assert (Array.length arr1 = Array.length arr2) *)
    let n = Array.length arr1 in
    let rec aux lst i =
      if i<n then
        aux ((arr1.(i),arr2.(i))::lst) (i+1)
      else lst
    in
      aux lst0 0

  let rec decompose b = function
    | []                                        -> Some2 b
    | (Var(_,k),p)::a | (p,Var (_,k))::a        -> decompose ((k,p)::b) a
    | (Pattern (md,id,args),
       Pattern(md',id',args'))::a       ->
        if ident_eq id id' && ident_eq md md'
                && Array.length args = Array.length args' then
          decompose b (add_to_list a args args')
        else None2

end

module PUnification = Make(PU)

let unify_p lst =
  match PUnification.unify lst with
    | Some2 s   -> Some s
    | None2     -> None
    | _         -> assert false
