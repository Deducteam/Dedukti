open Types

type position = int list

(* Rappel: Soient l1 -> r1, l2 -> r2 deux instances de rÃ¨gles de rÃ©Ã©criture
 * sans variables communes. Soit p une position de l1, tels que l1|p n'est pas
 * une variable et l1|p et l2 sont unifiables.
 *
 * Soit o l'unificateur principal de l1|p et l2.
 * Alors, la paire < o(r1) ; o(l1)[o(r2)]|p > est une paire critique.
 *      o(l1) --> o(r1)
 *      o(l1) --> o(l1)[o(r2)]|p
 *)

(* index1 indexes all lefthand side of rules by their head symbol *)
let index1 : (ident*ident,rule) Hashtbl.t = Hashtbl.create 147
let index1_get m v = Hashtbl.find_all index1 (m,v)
let index1_add r = Hashtbl.add index1 (r.md,r.id) r

(* index1 indexes all strict subpattern of lefthand side of rules
* by their head symbol *)
let index2 : (ident*ident,rule*position) Hashtbl.t = Hashtbl.create 147
let index2_get m v = Hashtbl.find_all index2 (m,v)
let index2_add m v r p = Hashtbl.add index2 (m,v) (r,p)

let rec replace_at_pos (p:position) (pat:pattern) (r:term) : term =
  match p, pat with
    | [], _                     -> r
    | k::p', Pattern(m,v,args)  ->
          let args2 =
            Array.init (Array.length args)
              (fun i -> if i=k then replace_at_pos p' args.(i) r
               else term_of_pattern_all_meta args.(i) )
          in
            mk_App ( (mk_Const m v)::(Array.to_list args2))
    | _,_                       -> assert false

let rec get_subpattern (p:position) (pat:pattern) : pattern =
  match p, pat with
    | [] , _                            -> pat
    | i::pos , Pattern (_,_,args)       ->
        ( assert (i<Array.length args) ; get_subpattern pos args.(i) )
    | _, _                              -> assert false

let get_top (r:rule) : pattern = Pattern (!Global.name,r.id,r.args)

let rec shift_pattern q = function
  | Var (n,k)            -> Var (n,k+q)
  | Pattern (md,id,args) -> Pattern (md,id,Array.map (shift_pattern q) args)

let shift_rule k r =
  { r with args = Array.map (shift_pattern k) r.args ;
           ri = Subst.shift k r.ri; }

let type_cpair red0 red1 red2 = 
  let eqs = Inference.infer_pattern2 red0 in
    match Unification.unify_t eqs with (*TODO not the right kind of unif*)
      | Failure _       -> assert false (*TODO*)
      | Success s       -> ( red1 , red2 ) (*TODO*)


(* Checks the potential critical pair r1 r2|p *)
let check_pair (l:loc) (r1:rule) (r2':rule) (pos:position) : unit =
  if ident_eq r1.md r2'.md && r1.nb<=r2'.nb && pos=[] then ()
  else
    let r2 = shift_rule r1.k r2' in (*renaming*)
    let le1 = get_top r1 in
    let le2 = get_top r2 in
      match Unification.unify_p [ (le1,get_subpattern pos le2) ] with
        | None      -> ()
        | Some s    ->
            (* Critical Pair from (s le2): < s (r2.ri) ; (s le2)[s (r1.ri)]|p > *)
            let s' = List.map (fun (i,p) -> (i,term_of_pattern_all_meta p)) s in
            let red0 = Subst.subst_pattern s le2 in
            let red1 = Subst.subst_meta s' r2.ri in
            let red2 = replace_at_pos pos (Subst.subst_pattern s le2)
                         (Subst.subst_meta s' r1.ri) in
            let (red1',red2') = type_cpair red0 red1 red2 in
            let cp =
              { rule1=r1.nb; rule2=r2.nb; pos=pos;
                root=red0; red1=red1'; red2=red2';
                joinable = Reduction.are_convertible red1' red2'; }
            in
              Global.warning r1.l (Pp.string_of_cpair cp)

let rec foreach_subpattern f pos = function (*FIXME pos*)
  | Var _                       -> ()
  | Pattern (md,id,args)        ->
      ( f pos md id; Array.iteri (fun i -> foreach_subpattern f (i::pos)) args )

let check_cpairs _ =
  (* Creation of Indexes *)
  Env.foreach_rule (
    fun r ->
      index1_add r ;
      Array.iteri
        (fun i ->
           foreach_subpattern (fun pos m' v' -> index2_add m' v' r pos) [i]
        ) r.args
  ) ;
  (* Critical Pair Detection *)
  Env.foreach_module_rule (
    fun r1 ->
      List.iter
        (fun r2 -> check_pair r1.l r1 r2 []) (index1_get r1.md r1.id) ;
      List.iter
        (fun (r2,p) -> check_pair r1.l r1 r2 p) (index2_get r1.md r1.id) ;
      Array.iteri (
        fun i -> foreach_subpattern (
          fun pos m' v' -> List.iter (
            fun r2 -> check_pair r1.l r2 r1 pos) (index1_get m' v')
      ) [i] ) r1.args
  ) ;
  (*Cleaning up*)
  Hashtbl.clear index1 ;
  Hashtbl.clear index2
