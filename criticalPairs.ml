
open Types

(*
        Rappel: Soient l1 -> r1, l2 -> r2 deux instances de règles de réécriture sans variables communes. 
        Soit p une position de l1, tels que l1|p n'est pas une variable et l1|p et l2 sont unifiables.
        Soit o l'unificateur principal de l1|p et l2. 
        Alors, la paire < o(r1) ; o(l1)[o(r2)]|p > est une paire critique.
                o(l1) --> o(r1)
                o(l1) --> o(l1)[o(r2)]|p
 *)

type position = int list 
type substitution = unit (*TODO*)

let index1_get_candidates (p:pattern) : (rule*position) list = [] (*TODO*)
let index2_get_candidates (p:pattern) : (rule*position) list = [] (*TODO*)

let rec index1_get_candidates_rec (lst:(rule*position) list) : pattern -> (rule*position) list = function
  | Pattern (_,_,args) as p     -> Array.fold_left index1_get_candidates_rec ((index1_get_candidates p)@lst) args
  | _                           -> lst

let unify (p1:pattern) (p2:pattern) : substitution option = assert false (*TODO*)
let subst (p:pattern) (s:substitution) : pattern = assert false (*TODO*)
let subst_t (p:term) (s:substitution) : term = assert false (*TODO*)
let replace_at_pos (p:position) (ctx:pattern) (r:term) : term = assert false (*TODO*)

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

let check_pair (l:loc) (r1:rule) (r2:rule) (p:position) = 
  let t1 = get_top r1 in
  let t2 = get_subpattern p (get_top r2) in
  match unify t1 t2 with
    | None      -> () (*False alert*)
    | Some s    -> 
        (* Critical Pair from (s t1): < s (r1.ri) ; (s (t1))[s (r2.ri)]|p > *)
        let pc_1 = subst_t r1.ri s in
        let pc_2 = replace_at_pos p (subst t1 s) (subst_t r2.ri s) in
        let join = Reduction.are_convertible pc_1 pc_2 in
          Global.warning l ( 
            "Rule Overlap between:\n" ^ Pp.string_of_rule r1 ^ "\n" ^ Pp.string_of_rule r2 
            ^ "\nCritical Pair:\n" ^ Pp.string_of_term pc_1 ^ "\n" ^ Pp.string_of_term pc_2
            ^ (if join then "\nJoinable" else "\nUNABLE TO JOIN!")
          )

let check (r1:rule) : unit =
  let top1 = get_top r1 in
  (* Trying to unify a subterm of top with an existing pattern*)
  let candidates1 = index1_get_candidates_rec [] top1 in
    List.iter ( fun (r2,p) -> check_pair r1.l r2 r1 p ) candidates1 ;
  (* Trying to unify top with a subterm of an existing pattern*)
  let candidates2 = index2_get_candidates top1 in
    List.iter ( fun (r2,p) -> check_pair r1.l r1 r2 p ) candidates2
