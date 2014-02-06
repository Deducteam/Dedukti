
open Types
(*
type substitution = (int*term)  list
type ustate = (term*term) list (* Terms to unify *)
            * (int*term)  list (* Variable to substitute *)
            * substitution 

let rec safe_assoc v = function
  | []                  -> None
  | (x,t)::_ when x=v   -> Some t
  | _::tl               -> safe_assoc v tl

let rec not_in n = function 
  | Meta k                      -> n<>k
  | Pi (_,a,b) | Lam (_,a,b)    -> not_in n a && not_in n b
  | App lst                     -> List.for_all (not_in n) lst
  | _                           -> true

let rec unify (lc:loc) : ustate -> substitution option = function
  | ( [] , [] , s )             -> Some s
  | ( [] , (v,t0)::b , s)       ->
      let t = Subst.subst_meta 0 s t0 in
        if not_in v t then ( 
          match safe_assoc v s with
            | Some t'   -> unify lc ( [(t,t')] , b , s )
            | None      -> 
                let s' = List.map ( fun (z,te) -> ( z , Subst.subst_meta 0 [(v,t)] te ) ) s in
                  unify lc ( [] , b , (v,t)::s' ) ) 
        else None
  | ( a , b , s )      -> 
      ( match Reduction.decompose_eq b a with
          | None        -> None
          | Some b'     -> unify lc ( [] , b' , s ) )
 *)

type 'a substitution = (int*'a) list
(*
type 'a option_ext =
  | None_ext
  | Some_ext of 'a
  | Maybe_ext
 *)
module type UTerm =
sig
  type term
  val subst : term substitution -> term -> term
  val occurs_check : int -> term -> bool
  val decompose :  (int*term) list -> (term*term) list -> ( (int*term) list ) option
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
                    let s' = List.map ( fun (z,t) -> ( z , T.subst [(v,te)] t ) ) s in
                      unify0 ( [] , lst , (v,te)::s' ) 
        end
    | ( a , b , s )      -> 
        begin
          match T.decompose b a with
            | None        -> None
            | Some b'     -> unify0 ( [] , b' , s )
        end

  let unify (lst: (T.term*T.term) list) : (T.term substitution) option = 
    unify0 ( lst, [], [] )
end

(* Partial Higher Order Unification *)

module TU : UTerm with type term = Types.term = 
struct

  type term = Types.term

  let subst = Subst.subst_meta 0

  let decompose = Reduction.decompose_eq

  let rec occurs_check n = function 
    | Meta k                      -> n=k
    | Pi (_,a,b) | Lam (_,a,b)    -> occurs_check n a || occurs_check n b
    | App lst                     -> List.exists (occurs_check n) lst
    | _                           -> false

end

module TUnification = Make(TU)

(* Pattern Typing *)
  
let rec check_meta = function
  | Meta _                      -> false
  | Pi (_,a,b) | Lam (_,a,b)    -> check_meta a && check_meta b
  | App lst                     -> List.for_all check_meta lst
  | _                           -> true

let resolve l id ty args eqs : term*pattern list = 
  match TUnification.unify eqs with
    | None      -> 
        let pat = Pp.string_of_pattern (Pattern(!Global.name,id,Array.of_list args)) in
          raise (PatternError (l, "The pattern '" ^ pat ^ "' is not well-typed." ))
    | Some s    -> 
        let ty'       = Subst.subst_meta 0 s ty in
        let args'     = List.map (Subst.subst_meta_p s) args in
          if check_meta ty' then
            ( ty' , args' )
          else
            let pat = Pp.string_of_pattern (Pattern(!Global.name,id,Array.of_list args')) in
              raise (PatternError (l,"Could not infer placeholders in the pattern '"^pat^"'."))

(* Pattern Unification *)

module PU : UTerm with type term = Types.pattern =
struct
  type term = Types.pattern
  let subst s p = assert false  (*TODO*)
  let occurs_check i p = assert false (*TODO*)
  let decompose b a = assert false (*TODO*)
end

module PUnification = Make(PU)

let pattern_unification (p1:pattern) (p2:pattern) : (pattern substitution) option =
  (*TODO variable renaming*)
  PUnification.unify [(p1,p2)]

