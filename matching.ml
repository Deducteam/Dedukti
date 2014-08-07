open Types


(* ******************* Matrix Definition ********************** *)

type pattern2 =
  | Var2         of Var.t
  | Pattern2     of ident*ident*pattern2 array

type rule2 =
    { loc:loc ;
      pats:pattern2 array ;
      var2idx: int VarMap.t; (* variable -> stack index *)
      right:term ;
      constraints:rule_constraint list ;
    }

(* a matrix is a non empty list of rule2
 * The length of the array args is the same for every rule *)

type matrix = {
  m_rules : rule2 list;
  m_arity : int;  (* arity of symbol that is de-structured *)
}

(*
let check_matrix (mx:matrix) : unit =
  match mx with
    | [] -> failwith "ill-formed matrix (empty list)."
    | r::lst ->
        List.iter (
          fun r' ->
            if Array.length r.pats != Array.length r'.pats then
              failwith "ill_formed matrix (different numbers of arguments)."
        ) lst
 *)

(* ******************* From rules to matrix ********************** *)

(* The goal of this transformation make the patterns linear and to record the
 * non-linearity and conditionnal rewriting constraints. *)

let br = hstring "{_}"
let un = hstring "_"

let to_rule2 (r:rule) : rule2 =
  let constraints = ref [] in
  (* store set of variables seen so far *)
  let seen = ref VarSet.empty in
  let var2idx = ref VarMap.empty in
  (* top: true iff the pattern isn't a sub-pattern *)
  let rec linearize ~top i = function
    | Var (l,v) ->
        if VarSet.mem v !seen
        then (
          let v' = Var.fresh v in
          constraints := (EqTerm (mk_Var dloc v, mk_Var dloc v')) :: !constraints ;
          Var2 v'
        ) else (
          (* remember where the variable is bound *)
          seen := VarSet.add v !seen;
          if top then var2idx := VarMap.add v i !var2idx;
          Var2 v
        )
    | Brackets t ->
        let v = Var.fresh_of_ident br in
        constraints := (EqTerm (mk_Var dloc v, t)) :: !constraints;
        Var2 v
    | Pattern (_,m,v,args) ->
        Pattern2(m,v, Array.of_list (List.mapi (linearize ~top:false) args))
    | Joker (l,_) ->
        let v = Var.fresh_of_ident un in
        Var2 v
  in
  let args = List.mapi (linearize ~top:true) r.args in
  { loc=r.l ; pats=Array.of_list args ; right=r.rhs ;
    constraints= !constraints ; var2idx= !var2idx}

(* build a matrix from rule2 list *)
let mk_matrix rules2 =
  match rules2 with
  | [] -> assert false
  | r::rs ->
      {m_arity = Array.length r.pats; m_rules = rules2; }

(* Construction of a matrix from a non empty list of rules.
 * It is checked that all the head symbol are the same and that all arities are
 * the same. *)

let matrix_of_rules : rule list -> matrix = function
  | [] -> assert false
  | r1::rs ->
      let r1' = to_rule2 r1 in
      let n = Array.length r1'.pats in
      let rs' =
        List.map (
          fun r2 ->
              if not (ident_eq r1.id r2.id) then
                Global.fail r2.l "Unexpected head symbol '%a' \
                  (expected '%a')." pp_ident r2.id pp_ident r1.id
              else
                let r2' = to_rule2 r2 in
                  if n != Array.length r2'.pats then
                    Global.fail r2.l "All the rewrite rules for \
                      the symbol '%a' should have the same arity." pp_ident r1.id
                  else r2'
        ) rs
      in
      mk_matrix (r1' :: rs')

(* ******************* Matrix manipulation ********************** *)

let _v = hstring "_"

let shift_var2idx var2idx c =
  VarMap.map
    (fun i -> if i > c then i-1 else i)
    var2idx

(* Remove column [c] in [line] and append [n] fresh variables.
* the right-hand side and the constraints are updated to depends
* on these new variables instead of the removed variable in column c*)
let specialize_var (c:int) (m:ident) (v:ident) (n:int) (line:rule2) (var:Var.t) =
  let old_n = Array.length line.pats in
  let new_n = old_n + n - 1 in
  (*    assert ( c < old_n ); *)
  (*    assert ( 0 <= new_n ); *)
  let new_vars = ref [] in
  let var2idx  = ref (shift_var2idx (VarMap.remove var line.var2idx) c) in
  let new_pats =
    Array.init new_n (
      fun i ->
        if i < c              then line.pats.(i)
        else if i < (old_n-1) then line.pats.(i+1)
        else (
          let fresh_v = Var.fresh_of_ident _v in
          new_vars := (mk_Var dloc fresh_v) :: !new_vars;
          var2idx := VarMap.add fresh_v (i+old_n-1) !var2idx;
          Var2 fresh_v
        )
    ) in
  let u = mk_App_l (mk_Const dloc m v) (List.rev !new_vars) in
  { line with
    pats  = new_pats ;
    constraints = List.rev_map
      (fun (EqTerm (t1,t2)) ->
        EqTerm (Subst.subst t1 ~var ~by:u, Subst.subst t2 ~var ~by:u)
      ) line.constraints ;
    right = Subst.subst line.right ~var ~by:u;
    var2idx = !var2idx;
  }

(* Remove column [c] in [line] and append [args] *)
let specialize_pat (c:int) (line:rule2) (args:pattern2 array) : rule2 =
  let n = Array.length args in
  let old_n = Array.length line.pats in
  let new_n = old_n + n - 1 in
  (*    assert ( c < old_n ); *)
  (*    assert ( 0 <= new_n ); *)
  let new_pats =
    Array.init new_n (
      fun i ->
        if i < c              then  line.pats.(i)   (* [ 0 - (c-1) ] *)
        else if i < (old_n-1) then  line.pats.(i+1) (* [ c - (n-2) ] *)
        else (* i < new_n *) args.(i-old_n+1)       (* [ (n-1) - (n+nargs-2) ] *)
    ) in
  (* update var2idx with new variables; also shift indices above c by one *)
  let var2idx = ref (shift_var2idx line.var2idx c) in
  Array.iteri
    (fun i pat -> match pat with
      | Var2 v -> var2idx := VarMap.add v (i+old_n-1) !var2idx;
      | _ -> ()
    ) args;
  { line with pats = new_pats; var2idx = !var2idx; }

(* Specialize the matrix [mx] to match a pattern headed by [m].[v] in column [c] ie:
*  - we forget the line with other patterns on line c;
*  - we get rid of the column c;
*  - we add the [nb_args] columns corresponding to the arguments of the pattern. *)
let specialize c mx (m,v,nb_args) =
  let aux lst li =
    ( match li.pats.(c) with
        | Pattern2 (m',v',args) ->
            if ident_eq v v' && ident_eq m m' then (specialize_pat c li args)::lst
            else lst
        | Var2 var -> (specialize_var c m v nb_args li var)::lst
    ) in
  let lst = List.fold_left aux [] mx.m_rules in
  nb_args, m, v, mk_matrix (List.rev lst)


(* Selects all the lines matching anything on column [c] *)
let default c mx =
  let m_rules = List.filter
    (fun li -> match li.pats.(c) with
      | Var2 _ -> true
      | _ -> false
    ) mx.m_rules in
  {mx with m_rules}

let eq m v (m',v',_) = ident_eq v v' && ident_eq m m'

(* Partition the matrix [mx] into:
 * - a new matrix specialized for each head symbol of pattern in column [c];
 * - a default matrix
 * each specialized matrix is coupled with a symbol+arity *)
let partition (c:int) (mx:matrix) : (int*ident*ident*matrix) list * matrix =
  let aux cstr li =
    match li.pats.(c) with
      | Pattern2 (m,v,args)      ->
          if List.exists (eq m v) cstr then cstr
          else (m,v,Array.length args)::cstr
      | Var2 _ -> cstr
  in
  let hsymbs = List.fold_left aux [] mx.m_rules in
  let cases  = List.map (specialize c mx) hsymbs in
  let def    = default c mx in
  cases, def

(* ******************* From matrix to dtree ********************** *)


(* Give the index of the first non variable column *)
let choose_col (line:pattern2 array) : int option =
  let rec aux i =
    if i < Array.length line
    then (
      match line.(i) with
      | Pattern2 (_,_,_) -> Some i
      | Var2 _ -> aux (i+1)
    ) else None
  in aux 0

(* Construct a decision tree out of a matrix *)
let rec to_dtree (mx:matrix) : dtree =
  let f_col, tail = match mx.m_rules with
    | f::tl -> (f,tl)
    | _ -> assert false
  in
  match choose_col f_col.pats with
    | None ->
        (* Only variables on the first line of the matrix *)
        (* def: subtree used to discriminate further *)
        let def = match tail with
          | li::_ ->
              if f_col.constraints = [] then
                ( Global.debug 1 li.loc "Useless rule." ; None )
              else Some (to_dtree {mx with m_rules=tail} )
          | []    -> None
        in
        (* how to build the substitution for testing? *)
        let subst_builder =
          VarMap.fold (fun v i acc -> (i,v)::acc)
          f_col.var2idx [] in
        Test (subst_builder, f_col.constraints, f_col.right, def )
    | Some c    ->
        (* Pattern on the first line at column c *)
        let (mx_cases,mx_def) = partition c mx in
        let def = match mx_def.m_rules with
          | []      -> None
          | _::_    -> Some (to_dtree mx_def)
        in
        let aux (n,m,v,mx) = (n,m,v,to_dtree mx) in
        Switch ( c , List.rev_map aux mx_cases , def )

(* ******************* Entry ********************** *)

let add_rules (rwi:rw_infos) (rs:rule list (*non empty*) ) : rw_infos =
  let ( ty , rules ) = match rwi with
    | Decl ty                   -> ( ty , rs )
    | Decl_rw (ty,mx,_,_)       -> ( ty , mx@rs )
    | Def (_,_)                 ->
        let r = match rs with r::_ -> r | _ -> assert false in
          Global.fail r.l "Cannot add rewrite\
            rules for the defined symbol '%a'." pp_ident r.id
  in
  let mx = matrix_of_rules rules in
  Decl_rw (ty, rules, mx.m_arity, to_dtree mx )
