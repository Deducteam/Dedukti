
open Types
open Unification

(* Rappel: Soient l1 -> r1, l2 -> r2 deux instances de règles de réécriture 
 * sans variables communes. Soit p une position de l1, tels que l1|p n'est pas
 * une variable et l1|p et l2 sont unifiables.
 * 
 * Soit o l'unificateur principal de l1|p et l2. 
 * Alors, la paire < o(r1) ; o(l1)[o(r2)]|p > est une paire critique.
 *      o(l1) --> o(r1)
 *      o(l1) --> o(l1)[o(r2)]|p
 *)


(*
* Table de Hash 1 (head symbol -> ensemble de (term,position,rule))
* Table de Hash 2 (head symbol -> ensemble de (term,rule)
* *)

type position = int list 


let find_all hs m v = 
  assert (ident_eq m !Global.name); (*FIXME*)
  Hashtbl.find_all hs v

(* Index de tout les pattern *)
let index1 : (ident,rule) Hashtbl.t = Hashtbl.create 147

(* Index de tout les pattern et sous-pattern *)
let index2 : (ident,rule*position) Hashtbl.t = Hashtbl.create 147

let clear () = (*FIXME*)
  Hashtbl.clear index1 ;
  Hashtbl.clear index2

let rec update_index2 r pos = function
  | Pattern (m,v,args)          -> 
      begin
        assert (ident_eq m !Global.name); (*FIXME*)
        Hashtbl.add index2 v (r,pos) ;
        Array.iteri (fun i p -> update_index2 r (i::pos) p) args
      end
  | _                   -> () (*FIXME*)

let index2_get_candidates = function
  | Pattern (m,v,_)     -> find_all index2 m v
  | _                   -> assert false

(* val foldi (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a *)
let foldi f a0 arr =
  snd ( Array.fold_left (fun (i,a) b -> ( i+1 , f i a b ) ) (0,a0) arr )

let rec index1_get_candidates pos lst : pattern -> (rule*position) list = function
  | Pattern (m,v,args)  -> 
      let cnds = List.map (fun r -> (r,pos)) (find_all index1 m v) in
        foldi (fun i lst0 pat -> index1_get_candidates (i::pos) lst0 pat) 
          (cnds@lst) args
  | _                   -> lst

let rec replace_at_pos (p:position) (pat:pattern) (r:term) : pattern = 
  match p, pat with
    | [], _                     -> Dot r
    | i::p', Pattern(m,v,args)  ->
        begin
          let args2 = Array.copy args in
            args2.(i) <- replace_at_pos p' args.(i) r ;
            Pattern (m,v,args2)
        end
    | _,_                       -> assert false 

let tsubst_of_psubst (s:pattern substitution) : term substitution = 
  List.map (fun (i,p) -> (i,term_of_pattern p)) s

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
          let pc_1 = Subst.subst_meta s' r2.ri in
          let pc_2_p = replace_at_pos p (Subst.subst_pattern s le2) 
                         (Subst.subst_meta s' r1.ri) in
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
  let candidates1 = index1_get_candidates [] [] top1 in
    List.iter ( fun (r2,p) -> check_pair r1.l r2 r1 p ) candidates1 ;
  (* Trying to unify top with a subterm of an existing pattern*)
  let candidates2 = index2_get_candidates top1 in
    List.iter ( fun (r2,p) -> check_pair r1.l r1 r2 p ) candidates2 ;
    Hashtbl.add index1 r1.id r1 ;
    update_index2 r1 [] top1
  
