
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

let rec replace_at_pos (p:position) (pat:pattern) (r:term) : term =  
  match p, pat with
    | [], _                     -> r
    | k::p', Pattern(m,v,args)  ->
          let args2 = 
            Array.init (Array.length args) 
              (fun i -> if i=k then replace_at_pos p' args.(i) r 
               else term_of_pattern args.(i) )
          in
            mk_App ( (mk_Const m v)::(Array.to_list args2))
    | _,_                       -> assert false 

let tsubst_of_psubst (s:pattern substitution) : term substitution = 
  List.map (fun (i,p) -> (i,term_of_pattern p)) s

let rec get_subpattern (p:position) (pat:pattern) : pattern = 
  match p, pat with
    | [] , _                            -> pat
    | i::pos , Pattern (_,_,args)       ->
        ( assert (i<Array.length args) ; get_subpattern pos args.(i) )
    | _, _                              -> assert false
 
let get_top (r:rule) : pattern = Pattern (!Global.name,r.id,r.args)

let rec max_index = function (*FIXME*)
  | Var (_,k)           -> k
  | Pattern (_,_,args)  -> 
      Array.fold_left 
        (fun (k0:int) (arg:pattern) -> max k0 (max_index arg)) (-1) args

let rec shift_pattern q = function (*FIXME*)
  | Var (n,k)            -> Var (n,k+q)
  | Pattern (md,id,args) -> Pattern (md,id,Array.map (shift_pattern q) args)

let shift_rule k r = (*FIXME*) 
  { r with args = Array.map (shift_pattern k) r.args ;
           ri = Subst.shift k 0 r.ri; }

let is_trivial a b = function
  | []  -> a=b
  | _   -> false

(* Checks the potential critical pair r1 r2|p *)
let check_pair (l:loc) (r1:rule) (rr2:rule) (p:position) (lst:cpair list) : cpair list = 
  if is_trivial r1.nb rr2.nb p then lst (*FIXME*)
  else (

  (* renaming FIXME*)
  let le1 = get_top r1 in 
  let k = (max_index le1) + 1 in (*FIXME*)
  let r2 = shift_rule k rr2 in (*FIXME*)

  let le2 = get_top r2 in 
    match unify_p [ (le1,get_subpattern p le2) ] with
      | None      -> lst (*False alert*)
      | Some s    -> 

          (* Critical Pair from (s le2): < s (r2.ri) ; (s le2)[s (r1.ri)]|p > *)
          let s' = tsubst_of_psubst s in
          let pat_0 = Subst.subst_pattern s le2 in (*FIXME is this well-typed ?*)
          let pc_1 = Subst.subst_var s' r2.ri in
          let pc_2 = (replace_at_pos p (Subst.subst_pattern s le2) (Subst.subst_var s' r1.ri)) in
          
          let pc = 
            { rule1=r1.nb;
              rule2=r2.nb;
              pos=p;
              root=pat_0; 
              red1=pc_1; 
              red2=pc_2; 
              joinable = Reduction.are_convertible pc_1 pc_2; (*FIXME*)
            } in

            pc :: lst )

let check_rule (r1:rule) : unit =
  let top1 = get_top r1 in
  (* Trying to unify a subterm of top with an existing pattern*)
  let candidates1 = index1_get_candidates [] [] top1 in
  let l1 = List.fold_left 
             ( fun lst (r2,p) -> check_pair r1.l r2 r1 p lst ) [] candidates1 in
  (* Trying to unify top with a subterm of an existing pattern*)
  let candidates2 = index2_get_candidates top1 in
  let l2 = List.fold_left 
             ( fun lst (r2,p) -> check_pair r1.l r1 r2 p lst ) l1 candidates2 in 
    List.iter 
      (fun cp -> 
         if not cp.joinable then Global.warning r1.l (Pp.string_of_cpair cp)) l2 (*FIXME*)
 
let index r = (*FIXME*)
  let top1 = get_top r in
  Hashtbl.add index1 r.id r ;
  update_index2 r [] top1

let check_for_cpair (lst:rule list) : unit = 
  List.iter index lst ;
  List.iter check_rule lst
  
