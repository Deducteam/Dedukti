
open Types

type 'a substitution = (int*'a) list

module type UTerm =
sig
  type term
  val subst: term substitution -> term -> term
  val occurs_check: int -> term -> bool
  val decompose: (int*term) list -> (term*term) list -> ((int*term) list) option
end

module Make = functor (T: UTerm) ->
struct
  type state = (T.term*T.term) list * (int*T.term) list * T.term substitution

  let rec safe_assoc v = function
  | []                  -> None
  | (x,t)::_ when x=v   -> Some t
  | _::tl               -> safe_assoc v tl

  let rec unify0 : state -> (T.term substitution) option = function
    | ( [], [] , s )            -> Some s
    | ( [], (v,te0)::lst , s )  ->
        begin
          let te = T.subst s te0 in
            if T.occurs_check v te then None
            else
              match safe_assoc v s with
                | Some te'      -> unify0 ( [(te,te')] , lst , s )
                | None      -> 
                    let s' = 
                      List.map ( fun (z,t) -> ( z , T.subst [(v,te)] t ) ) s 
                    in
                      unify0 ( [] , lst , (v,te)::s' ) 
        end
    | ( a , b , s )      -> 
        begin
          match T.decompose b a with
            | None        -> None
            | Some b'     -> unify0 ( [] , b' , s )
        end

  let unify lst = unify0 ( lst , [] , [] )
end

(* Partial Higher Order Unification *)

module TU : UTerm with type term = Types.term = 
struct
  type term = Types.term
  let subst = Subst.subst_meta 
  let decompose = Reduction.decompose_eq
  let rec occurs_check n = function 
    | Meta k                      -> n=k
    | Pi (_,a,b) | Lam (_,a,b)    -> occurs_check n a || occurs_check n b
    | App lst                     -> List.exists (occurs_check n) lst
    | _                           -> false
end

module TUnification = Make(TU)

let unify_t = TUnification.unify

(* Pattern Typing *)
  
(* FIXME
let resolve l id ty args eqs : term*pattern list = 
  match TUnification.unify eqs with
    | None      -> 
        let pat = 
          Pp.string_of_pattern (Pattern(!Global.name,id,Array.of_list args)) 
        in
          raise (PatternError (l,"The pattern '" ^ pat ^ "' is not well-typed."))
    | Some s    -> 
        let ty'       = Subst.subst_meta s ty in
        let args'     = List.map (Subst.subst_pattern2 s) args in
          if check_meta ty' then
            ( ty' , args' )
          else
            let pat =
              Pp.string_of_pattern (Pattern(!Global.name,id,Array.of_list args')) 
            in
              raise (PatternError (l,"Could not infer placeholders in the pattern '"^pat^"'."))
 *)

(* Pattern Unification *)

module PU : UTerm with type term = Types.pattern =
struct
  type term = Types.pattern
  let subst = Subst.subst_pattern
  
  let rec occurs_check n = function
    | Joker k           -> n=k
    | Pattern(_,_,args) -> aux n args 0
    | _                 -> false
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
    | []                                -> Some b
    | (Joker k,p)::a | (p,Joker k)::a   -> decompose ((k,p)::b) a
    | (Var (_,i),Var (_,j))::a          -> if i=j then decompose b a else None
    | (Dot _,_)::a | (_,Dot _)::a       -> assert false (*FIXME*)
    | (Pattern (md,id,args),
       Pattern(md',id',args'))::a       ->
        if ident_eq id id' && ident_eq md md' 
                && Array.length args = Array.length args' then
          decompose b (add_to_list a args args')
        else None
    | _                                 -> None

end

module PUnification = Make(PU)

let unify_p lst =
  (*TODO variable renaming*)
  PUnification.unify lst

