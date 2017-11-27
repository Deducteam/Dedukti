open Basic
open Term
open Rule
open Format
open Matching

type dtree_error =
  | HeadSymbolMismatch  of loc * name * name
  | ArityDBMismatch     of loc * name * int
  | AritySymbolMismatch of loc * name * name

exception DtreeExn of dtree_error

(* A subtype of rule_infos with only the informations that the dtree need *)
type dtree_rule =
  {
    loc:loc ;
    pid:name;
    pats:wf_pattern array ;
    right:term ;
    constraints:constr list ;
    esize:int ;
  }

let to_dtree_rule (r:rule_infos) : dtree_rule =
  {
    loc = r.l ;
    pid = r.cst;
    pats = [| LPattern (r.cst, r.l_args) |]  ;
    right = r.rhs ;
    constraints = r.constraints ;
    esize = r.esize ;
  }

(*
 * there is one matrix per head symbol that represents all the rules associated to that symbol.
 * col_depth:   [ (n_0)          (n_1)          ...       (n_k)   ]
 * first:       [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 * others:      [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 *                  ...           ...           ...       ...
 *              [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 *
 *              n_i records the depth of the column (number of binders under which it stands)
 *)
type matrix =
    { col_depth: int array;
      first:dtree_rule ;
      others:dtree_rule list ; }

(* Remove a line of the matrix [mx] and return None if the new matrix is Empty. *)
let pop mx =
  match mx.others with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

let split_mx (f:dtree_rule -> bool) (mx:matrix) : matrix option * matrix option =
  let (l1,l2) = List.partition f (mx.first::mx.others) in
  let aux = function
    | [] -> None
    | f::o -> Some { col_depth=mx.col_depth; first=f; others=o; }
  in
  (aux l1, aux l2)

let filter (f:dtree_rule -> bool) (mx:matrix) : matrix option =
  match List.filter f (mx.first::mx.others) with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

let get_rule_filter f c r = f r.pats.(c)

(* Keeps only the rules with a lambda on column [c] *)
let filter_on_lambda = function
  | LLambda _ | LJoker | LVar _ -> true
  | LACSet (_,s) -> List.exists (function LLambda _ -> true | _ -> false) s
  | _ -> false

(* Keeps only the rules with a bound variable of index [n] on column [c] *)
let filter_on_bound_variable nargs n = function
  | LVar _ | LJoker -> true
  | LBoundVar (_,n',args) -> (n' == n && Array.length args == nargs)
  | LACSet(_,s) -> assert false
  | _ -> false

(* Keeps only the rules with a pattern head by [m].[v] on column [c] *)
let filter_on_pattern nargs cst = function
  | LVar _ | LJoker -> true
  | LPattern (cst',ar') -> (name_eq cst cst' && Array.length ar' == nargs)
  | LACSet (_,s) -> assert false
  | _ -> false



let partition_AC_rules c f rules =
  let rec aux (keep,def) = function
    | [] -> (keep,def)
    | r :: tl ->
      match r.pats.(c) with
      | LVar _ | LJoker -> aux (r :: keep, r :: def) tl
      | LACSet (_,pats) ->
        if f pats
        then aux (r::keep,    def) tl
        else aux (   keep, r::def) tl
      | _ -> aux (keep, r::def) tl
  in
  aux ([],[]) rules

let filter_AC_on_empty_set = function
  | LACSet (_,[]) -> true
  | _ -> false

let filter_AC_on_lambda s = List.exists (function LLambda _ -> true | _ -> false) s

let filter_AC_on_bound_variable nargs n s =
  List.exists
    (function
      | LBoundVar (_,n',args) -> (n' == n && Array.length args == nargs)
      | _ -> false)
    s

let filter_AC_on_pattern nargs cst s =
  List.exists (function
      | LPattern (cst',ar') -> (name_eq cst cst' &&
                                Array.length ar' == nargs)
      | _ -> false)
    s

type case =
  | CConst   of int * name * bool
  | CDB      of int * int
  | CLam

type dtree =
  | Fetch   of int * case * dtree * dtree option
  | ACEmpty of int * dtree * dtree option
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of pre_matching_problem * constr list * term * dtree option

let pp_AC_args fmt i =
  fprintf fmt (if i > 2 then "%i args, first 2 AC flattened" else "%i args") i

let rec pp_dtree t fmt dtree =
  (* FIXME: Use format boxex here instead of manual tabs. *)
  let tab = String.init (1 + t*4) (fun i -> if i == 0 then '\n' else ' ') in
  match dtree with
  | Test (mp,[],te,def) when List.length mp.pm_problems == 0 -> fprintf fmt "%s%a" tab pp_term te
  | Test (mp,[],te,def)  ->
     fprintf fmt "%stry %a%sthen %a%selse %a"
             tab (pp_pre_matching_problem (tab^"      ")) mp tab pp_term te tab (pp_def (t+1)) def
  | Test (mp,cstr,te,def)  ->
     fprintf fmt "%stry %a%sunder constraints %a%sthen %a%selse %a"
             tab (pp_pre_matching_problem (tab^"      ")) mp tab (pp_list ", " pp_constr) cstr
             tab pp_term te tab (pp_def (t+1)) def
  | ACEmpty (i,tree_suc,tree_def) ->
     fprintf fmt "%sif $%i (AC flattened) is empty then %a%selse %a"
             tab i (pp_dtree (t+1)) tree_suc tab (pp_def (t+1)) tree_def
  | Fetch (i,case,tree_suc,tree_def) ->
     (match case with
       | CConst (nargs,name,false) ->
          fprintf fmt "%sif $%i is AC applied to %a (%i args) then %a%selse %a"
                  tab i pp_name name nargs
       | CConst (nargs,name,true) ->
          fprintf fmt "%sif $%i is AC applied to %a (%a) then %a%selse %a"
                  tab i pp_name name pp_AC_args nargs
       | CDB (nargs,n) ->
          fprintf fmt "%sif $%i is AC applied to DB[%i] (%i args) then %a%selse %a"
                  tab i n nargs
       | CLam -> fprintf fmt "%sif $%i is AC applied to Lambda then %a%selse %a" tab i
     ) (pp_dtree (t+1)) tree_suc tab (pp_def (t+1)) tree_def
  | Switch (i,cases,def) ->
     let pp_case out = function
       | CConst (nargs,name,false), g ->
          fprintf out "%sif $%i is %a (%i args) then %a"
                  tab i pp_name name nargs (pp_dtree (t+1)) g
       | CConst (nargs,name,true), g ->
          fprintf out "%sif $%i=%a (%a) then %a"
                  tab i pp_name name pp_AC_args nargs (pp_dtree (t+1)) g
       | CDB (nargs,n), g ->
          fprintf out "%sif $%i=DB[%i] (%i args) then %a"
                  tab i n nargs (pp_dtree (t+1)) g
       | CLam, g -> fprintf out "%sif $%i=Lambda then %a" tab i (pp_dtree (t+1)) g
     in
     fprintf fmt "%a%sdefault: %a" (pp_list "" pp_case) cases tab (pp_def (t+1)) def

and pp_case fmt = function
  | CConst (nargs,cst,false) ->
     fprintf fmt "CConst %a (%i args)" pp_name cst nargs
  | CConst (nargs,cst,true) ->
     fprintf fmt "AC CConst %a (%a)" pp_name cst pp_AC_args nargs
  | CDB (nargs,n) -> fprintf fmt "DB[%i] (%i args)" n nargs
  | CLam -> fprintf fmt "Lambda"

and pp_def t fmt = function
  | None   -> fprintf fmt "FAIL"
  | Some g -> pp_dtree t fmt g

let pp_dtree fmt dtree = pp_dtree 0 fmt dtree

let eq a b =
  match a, b with
    | CLam              , CLam                  -> true
    | CDB    (ar,n)     , CDB    (ar',n')       -> ar == ar' && n == n'
    | CConst (ar,cst,ac), CConst (ar',cst',ac') -> ar == ar' && name_eq cst cst'
    | _, _ -> false

let case_of_pattern (is_AC:name->bool) : wf_pattern -> case option = function
  | LVar _ | LJoker      -> None
  | LPattern (cst,pats)  -> Some (CConst (Array.length pats,cst,
                                          is_AC cst && Array.length pats >= 2))
  | LBoundVar (_,n,pats) -> Some (CDB (Array.length pats,n))
  | LLambda _            -> Some CLam
  | LACSet _ -> assert false

let case_pattern_match (case:case) (pat:wf_pattern) : bool = match case, pat with
  | CConst (lpats,c',_), LPattern  (c,pats)   -> name_eq c c' &&
                                                   lpats == Array.length pats
  | CDB    (lpats,n')  , LBoundVar (_,n,pats) -> n' == n && lpats == Array.length pats
  | CLam               , LLambda _            -> true
  | _ -> false

let flatten_pattern cst pats =
  let rec flatten acc = function
  | [] -> acc
  | LPattern(cst',args)::tl when name_eq cst cst' && Array.length args == 2 ->
     flatten acc (args.(0) :: args.(1) :: tl)
  | t :: tl -> flatten (t::acc) tl
  in
  flatten [] pats

let specialize_empty_AC_rule (c:int) (r:dtree_rule) : dtree_rule =
  { r with pats = Array.init (Array.length r.pats)
                             (fun i -> if i==c then LJoker else r.pats.(i))  }

let specialize_AC_rule case (c:int) (nargs:int) (r:dtree_rule) : dtree_rule =
  let size = Array.length r.pats in
  let new_pats_c, pat =
    match r.pats.(c) with
    | LACSet (cst,l) ->
       let rec remove_case acc = function
         | [] -> assert false
         | hd :: tl -> if case_pattern_match case hd
                       then LACSet (cst,List.rev_append acc tl), hd
                       else remove_case (hd::acc) tl
       in
       remove_case [] l
    | LVar _ | LJoker -> r.pats.(c), LJoker
    | _ -> assert false in
  let aux i =
    if i < size then
      if i==c then new_pats_c else r.pats.(i)
    else (* size <= i < size+nargs *)
      match pat, case with
      | LPattern (cst,pats2), CConst(nargs',cst',true) ->
         assert(name_eq cst cst');
         assert(nargs >= 1);
         assert(Array.length pats2 == (nargs+1) );
         if i == size
         then LACSet(cst, flatten_pattern cst [pats2.(0);pats2.(1)])
         else pats2.(i - size + 1)
      | LPattern  (_,pats2),_
      | LBoundVar (_,_,pats2),_ ->
         assert ( Array.length pats2 == nargs );
         pats2.(i - size)
      | LLambda (_,p),_ -> ( assert ( nargs == 1); p )
      | LJoker,_ -> LJoker
      | _ -> assert false
  in
  { r with pats = Array.init (size+nargs) aux }

(* Specialize the rule [r] on column [c]
 * i.e. remove colum [c] and append [nargs] new column at the end.
 * These new columns contain
 * - the arguments if column [c] is a pattern
 * - or the body if column [c] is a lambda
 * - or Jokers otherwise
* *)
let specialize_rule case (c:int) (nargs:int) (r:dtree_rule) : dtree_rule =
  let size = Array.length r.pats in
  let aux i =
    if i < size then
      if i==c then
        match r.pats.(c) with
        | LVar _ as v -> v
        | _ -> LJoker
      else r.pats.(i)
    else (* size <= i < size+nargs *)
      match r.pats.(c), case with
        | LJoker,_ | LVar _,_ -> LJoker
        | LPattern (cst,pats2), CConst(nargs',cst',true) ->
           assert(name_eq cst cst');
           assert(Array.length pats2 == (nargs+1) && nargs != 0);
           if i == size
           then LACSet(cst, (flatten_pattern cst [pats2.(0);pats2.(1)]))
           else pats2.(i - size + 1)
        | LACSet _, _ -> assert false
        | LPattern(_,pats2),_
        | LBoundVar(_,_,pats2),_ ->
           assert (Array.length pats2 == nargs);
           pats2.( i - size)
        | LLambda (_,p),_ -> assert (nargs == 1); p
  in
  { r with pats = Array.init (size+nargs) aux }

(* Specialize the col_infos field of a matrix.
 * Invalid for specialization by lambda. *)
let spec_col_depth (c:int) (nargs:int) (col_depth: int array) : int array =
  let size = Array.length col_depth in
  let aux i =
    if i < size then col_depth.(i)
    else (* < size+nargs *) col_depth.(c)
  in
    Array.init (size+nargs) aux

(* Specialize the col_infos field of a matrix: the lambda case. *)
let spec_col_depth_l (c:int) (col_depth: int array) : int array =
  let size = Array.length col_depth in
  let aux i =
    if i < size then col_depth.(i)
    else (*i == size *) col_depth.(c) + 1
  in
    Array.init (size+1) aux

(* Keeps only the rules with a joker or a variable on column [c] *)
let filter_default (mx:matrix) (c:int) : matrix option =
  filter (
    fun r -> match r.pats.(c) with
      | LVar _ | LJoker -> true
      | LLambda _ | LPattern _  | LBoundVar _ -> false
      | LACSet _ -> assert false
  ) mx

(* Specialize the matrix [mx] on column [c] *)
let specialize_ACEmpty (mx:matrix) (c:int) : matrix * matrix option =
  let (rules_suc, rules_def) =
    List.partition (get_rule_filter filter_AC_on_empty_set c) (mx.first::mx.others) in
  match rules_suc with
  | [] -> assert false
  | first::others ->
     { mx with
       first =            specialize_empty_AC_rule c first;
       others = List.map (specialize_empty_AC_rule c ) others
     },
     (match rules_def with
      | [] -> None
      | f::o -> Some ({ mx with first = f; others = o}))

(* Specialize the matrix [mx] on column [c] *)
let specialize_AC (mx:matrix) (c:int) (case:case) : matrix * matrix option =
  let nargs, part_f = match case with
    | CLam                 -> 1    , filter_AC_on_lambda
    | CDB    (nargs,n)     -> nargs, filter_AC_on_bound_variable nargs n
    | CConst (nargs,cst,_) -> nargs, filter_AC_on_pattern        nargs cst  in
  let rules_suc, rules_def = partition_AC_rules c part_f (mx.first::mx.others) in
  let add_args = nargs - (match case with CConst(_,_,true) -> 1 | _ -> 0) in
  let new_cn = match case with
    | CLam -> spec_col_depth_l c          mx.col_depth
    | _    -> spec_col_depth   c add_args mx.col_depth  in
  match rules_suc with
  | [] -> assert false
  | first::others ->
     { first =            specialize_AC_rule case c add_args  first;
       others = List.map (specialize_AC_rule case c add_args) others;
       col_depth = new_cn;
     },
     (match rules_def with
      | [] -> None
      | f::o -> Some ({ mx with first = f; others = o}))

(* Specialize the matrix [mx] on column [c] *)
let specialize (mx:matrix) (c:int) (case:case) : matrix =
  let nargs, filter_f = match case with
    | CLam                   -> 1    , filter_on_lambda
    | CDB      (nargs,n)     -> nargs, filter_on_bound_variable nargs n
    | CConst   (nargs,cst,_) -> nargs, filter_on_pattern        nargs cst
  in
  let mx_opt = filter (get_rule_filter filter_f c) mx in
  let add_args = nargs - (match case with CConst(_,_,true) -> 1 | _ -> 0) in
  let new_cn = match case with
    | CLam                -> spec_col_depth_l c          mx.col_depth
    | _                   -> spec_col_depth   c add_args mx.col_depth
  in
  match mx_opt with
  | None -> assert false
  | Some mx2 ->
     { first =            specialize_rule case c add_args  mx2.first;
       others = List.map (specialize_rule case c add_args) mx2.others;
       col_depth = new_cn;
     }

(* ***************************** *)

let rec partition_AC (is_AC:name->bool) : wf_pattern list -> case = function
  | [] -> assert false
  | hd :: tl ->
    match case_of_pattern is_AC hd with
    | Some c -> c
    | None   -> partition_AC is_AC tl

let partition (ignore_arity:bool) (is_AC:name->bool) (mx:matrix) (c:int) : case list =
  let aux lst li =
    let compare_case =
      if ignore_arity then eq
      else fun c1 c2 ->
           match c1, c2 with
           | CDB(ar,n), CDB(ar',n') when n == n' && ar != ar' ->
              raise (DtreeExn (ArityDBMismatch(li.loc, li.pid, n)))
           | CConst(ar,cst,ac), CConst(ar',cst',ac')
                when name_eq cst cst' && (ar != ar' || ac != ac') ->
              raise (DtreeExn (AritySymbolMismatch(li.loc, li.pid, cst)))
           | _ -> eq c1 c2
    in
    match case_of_pattern is_AC li.pats.(c) with
    | Some c -> if List.exists (compare_case c) lst then lst else c::lst
    | None   -> lst
  in
  List.fold_left aux [] (mx.first::mx.others)

(* ***************************** *)

let array_to_llist arr =
  LList.make_unsafe (Array.length arr) (Array.to_list arr)

let get_first_term        mx = mx.first.right
let get_first_constraints mx = mx.first.constraints

(* Extracts the matching_problem from the first line. *)
let get_first_matching_problem (get_algebra:name->algebra) mx =
  let esize = mx.first.esize in
  let miller = Array.make esize (-1) in
  let pbs = ref [] in
  Array.iteri
    (fun i p ->
      let depth = mx.col_depth.(i) in
      match p with
      | LJoker -> ()
      | LVar (_,n,lst) ->
         begin
           assert(depth <= n && n < esize + depth);
           let n = n - depth in
           let len = List.length lst in
           if miller.(n) == -1 then miller.(n) <- len else assert(miller.(n) == len);
           pbs := (depth, Eq( (n, LList.make len lst), i)) :: !pbs
         end
      | LACSet (cst,patl) ->
         begin
           let fetch_vars (joks,vars) = function
              | LJoker -> (joks+1,vars)
              | LVar (_,n,lst) ->
                 begin
                   assert(depth <= n && n < esize + depth);
                   let n = n - depth in
                   let len = List.length lst in
                   if miller.(n) == -1 then miller.(n) <- len else assert(miller.(n) == len);
                   let nvars = (n, LList.make len lst) :: vars in
                   (joks,nvars)
                 end
              | _ -> assert false in
           let njoks, vars = List.fold_left fetch_vars (0,[]) patl in
           pbs := (depth, AC((cst,get_algebra cst),njoks,vars,[i]) ) :: !pbs
         end
      | _ -> assert false
    ) mx.first.pats;
  assert (Array.fold_left (fun a x -> a && x >= 0) true miller);
  {
    pm_problems = !pbs;
    pm_miller = miller
  }

(******************************************************************************)

(*  TODO: check at some point that no neutral element can occur in a pattern *)
let rec non_var_pat = function
  | LVar _ | LJoker -> false
  | LACSet (_,[]) -> true
  | LACSet (_,patl) -> List.exists non_var_pat patl
  | _ -> true

(* Give the index of the first non variable column *)
let choose_column mx =
  let rec aux i =
    if i < Array.length mx.first.pats then
      if non_var_pat mx.first.pats.(i) then Some i else aux (i+1)
    else None
  in
  aux 0

(* Construct a decision tree out of a matrix *)
let rec to_dtree get_algebra (mx:matrix) : dtree =
  let is_AC cst = is_AC (get_algebra cst) in
  match choose_column mx with
    (* There are only variables on the first line of the matrix *)
  | None   -> Test ( get_first_matching_problem get_algebra mx,
                     get_first_constraints mx,
                     get_first_term mx,
                     map_opt (to_dtree get_algebra) (pop mx) )
  (* Pattern on the first line at column c *)
  | Some c ->
     match mx.first.pats.(c) with
     | LACSet (_,[]) ->
        let mx_suc, mx_def = specialize_ACEmpty mx c in
        ACEmpty (c, to_dtree get_algebra mx_suc, map_opt (to_dtree get_algebra) mx_def)
     | LACSet (_,l) ->
        let case = partition_AC is_AC l in
        let mx_suc, mx_def = specialize_AC mx c case in
        Fetch (c, case, to_dtree get_algebra mx_suc, map_opt (to_dtree get_algebra) mx_def)
     | _ ->
        (* Carry parameter (false) above  *)
        let cases = partition true is_AC mx c in
        let aux ca = ( ca , to_dtree get_algebra (specialize mx c ca) ) in
        Switch (c, List.map aux cases, map_opt (to_dtree get_algebra) (filter_default mx c) )

(******************************************************************************)

let of_rules (get_algebra:name->algebra) (rs:rule_infos list) : (dtree,dtree_error) error =
  try
    match rs with
    | [] -> assert false
    | r1 :: ro ->
      (*  Building a matrix out of the non-empty list of rules.
       *  It is checked that all rules have the same head symbol.  *)
      let to_dtree_r r2 =
        if not (name_eq r1.cst r2.cst)
        then raise (DtreeExn (HeadSymbolMismatch (r2.l,r2.cst,r1.cst)))
        else to_dtree_rule r2 in
      let o = List.map to_dtree_r ro in
      let mx = { first=(to_dtree_rule r1); others=o; col_depth=Array.make 1 0 ;} in
      OK (to_dtree get_algebra mx)
  with DtreeExn e -> Err e
