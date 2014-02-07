
open Types
open Unification

(*
        Rappel: Soient l1 -> r1, l2 -> r2 deux instances de règles de réécriture sans variables communes. 
        Soit p une position de l1, tels que l1|p n'est pas une variable et l1|p et l2 sont unifiables.
        Soit o l'unificateur principal de l1|p et l2. 
        Alors, la paire < o(r1) ; o(l1)[o(r2)]|p > est une paire critique.
                o(l1) --> o(r1)
                o(l1) --> o(l1)[o(r2)]|p
 *)


(*
 * Rules : int -> rule ??? demander Simon
* Table de Hash 1 (head symbol -> ensemble de (term,position,rule_id))
* Table de Hash 2 (head symbol -> ensemble de (term,rule_id)
*
* *)

type position = int list 

(* Index de tout les pattern *)
let index1_get_candidates (p:pattern) : (rule*position) list = [] (*TODO*)
(* Index de tout les pattern et sous-pattern *)
let index2_get_candidates (p:pattern) : (rule*position) list = [] (*TODO*)

let rec index1_get_candidates_rec lst = function
  | Pattern (_,_,args) as p     -> 
      Array.fold_left index1_get_candidates_rec ((index1_get_candidates p)@lst) args
  | _                           -> lst
(*
(* fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a *)
let fold_left2 f a arr1 arr2 =
  assert (Array.length arr1 = Array.length arr2) ;
  let rec aux i aa =
    if i < Array.length arr1 then
      aux (i+1) (f aa arr1.(i) arr2.(i))
    else
      aa
  in
    aux 0 a

type ip = int * pattern
type substitution = ip list
exception CannotUnify

let subst (p:pattern) (s:substitution) : pattern = assert false (*TODO*)
let subst_t (p:term) (s:substitution) : term = assert false (*TODO*)

let rec decompose lst (p1:pattern) (p2:pattern) : ip list = 
  match p1, p2 with
    | Joker _, _ | _, Joker _                   -> assert false (*FIXME*)
    | Dot _, _ | _, Dot _                       -> assert false (*FIXME*)
    | Var (_,i) , p | p, Var (_,i)              -> (i,p)::lst 
    | Pattern (m,v,args), Pattern (m',v',args') ->
        if ident_eq v v' && ident_eq m m' then
          fold_left2 decompose lst args args'
        else
          raise CannotUnify

let rec not_in i p = assert false

let rec resolve (c:ip list) (s:substitution) : substitution = assert false (* 
  match c with
    | []        -> s
    | (i,p)::c0 -> 
        let p' = subst p s in
          if not_in i p' then 
            let s' = List.map ( fun (j,q) -> (j,subst q [(i,p')] ) ) s in
              resolve c0 ((i,p')::(List.map s)
          else raise CannotUnify *)


let unify (p1:pattern) (p2:pattern) : substitution option = 
  assert false (* resolve ( decompose [] p1 p2 , [] ) *)
 *)

let subst_p (p:pattern) (s:pattern substitution) : pattern = 
  assert false (*TODO*)
let subst_t (p:term) (s:term substitution) : term = 
  assert false (*TODO*)
let replace_at_pos (p:position) (pat:pattern) (r:term) : pattern = 
  assert false (*TODO*)
let tsubst_of_psubst (s:pattern substitution) : term substitution = 
  assert false (*TODO*) 
let term_of_pattern _ = assert false (*TODO*) 

let rec get_subpattern (p:position) (pat:pattern) : pattern = 
  match p, pat with
    | [] , _                            -> pat
    | i::pos , Pattern (_,_,args)       ->
        begin
          assert (i<Array.length args) ; 
          get_subpattern pos args.(i) 
        end
    | _, _                              -> assert false
 
let get_top (r:rule) : pattern = Pattern (!Global.name,r.id,r.args)
 
(* Checks the potential critical pair r1 r2|p *)
let check_pair (l:loc) (r1:rule) (r2:rule) (p:position) : unit = 
  let le1 = get_top r1 in
  let le2 = get_top r2 in
    match unify_p [ (le1,get_subpattern p le2) ] with
      | None      -> () (*False alert*)
      | Some s    -> 
          (* Critical Pair from (s le2): < s (r2.ri) ; (s le2)[s (r1.ri)]|p > *)
          let s' = tsubst_of_psubst s in
          let pc_1 = subst_t r2.ri s' in
          let pc_2_p = replace_at_pos p (subst_p le2 s) (subst_t r1.ri s') in
          let pc_2 = term_of_pattern pc_2_p in
          let join = Reduction.are_convertible pc_1 pc_2 in
            Global.warning l ( "Rule Overlap between:\n" ^ Pp.string_of_rule r1 
                               ^ "\n" ^ Pp.string_of_rule r2 
                               ^ "\nCritical Pair:\n" 
                               ^ Pp.string_of_term pc_1 ^ "\n" 
                               ^ Pp.string_of_term pc_2
                               ^ (if join then "\nJoinable" 
                                  else "\nUNABLE TO JOIN!" )
            )

let check (r1:rule) : unit =
  let top1 = get_top r1 in
  (* Trying to unify a subterm of top with an existing pattern*)
  let candidates1 = index1_get_candidates_rec [] top1 in
    List.iter ( fun (r2,p) -> check_pair r1.l r2 r1 p ) candidates1 ;
  (* Trying to unify top with a subterm of an existing pattern*)
  let candidates2 = index2_get_candidates top1 in
    List.iter ( fun (r2,p) -> check_pair r1.l r1 r2 p ) candidates2
