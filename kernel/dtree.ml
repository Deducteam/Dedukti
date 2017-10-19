open Basic
open Term
open Rule
open Format

type dtree_error =
  | HeadSymbolMismatch of loc * ident * ident
  | ArityMismatch of loc * ident
  | ArityInnerMismatch of loc * ident * ident

exception DtreeExn of dtree_error

(* A subtype of rule_infos with only the informations that the dtree need *)
type dtree_rule =
  { loc:loc ;
    pid:ident;
    pats:wf_pattern array ;
    right:term ;
    constraints:constr list ;
    esize:int ; }

let to_dtree_rule (r:rule_infos) : dtree_rule =
  { loc = r.l ;
    pid = r.id;
    pats = [| LPattern (r.md, r.id, r.l_args) |]  ;
    right = r.rhs ;
    constraints = r.constraints ;
    esize = r.esize ; }

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

let filter_on_pattern_size size r =
  match r.pats.(0) with
  | LPattern(_,_,p) -> (Array.length p == size)
  | _ -> assert false

let filter (f:dtree_rule -> bool) (mx:matrix) : matrix option =
  match List.filter f (mx.first::mx.others) with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

(* Keeps only the rules with a lambda on column [c] *)
let filter_on_lambda c r =
  match r.pats.(c) with
  | LLambda _ | LJoker | LVar _ -> true
  | LACSet s -> List.exists (function LLambda _ -> true | _ -> false) s
  | _ -> false

(* Keeps only the rules with a bound variable of index [n] on column [c] *)
let filter_on_bound_variable c nargs n r =
  match r.pats.(c) with
  | LVar _ | LJoker -> true
  | LBoundVar (_,n',args) -> (n' == n && Array.length args == nargs)
  | LACSet s -> List.exists
                  (function
                   | LBoundVar (_,n',args) -> (n' == n && Array.length args == nargs)
                   | _ -> false)
                  s
  | _ -> false

let filter_on_variable_AC_set n c r =
  match r.pats.(c) with
  | LACSet l -> n == List.length l &&
                  List.for_all (function LJoker | LVar _ -> true | _ -> false) l
  | _ -> false
  
(* Keeps only the rules with a pattern head by [m].[v] on column [c] *)
let filter_on_pattern c nargs m v r =
  match r.pats.(c) with
    | LVar _ | LJoker -> true
    | LPattern (m',v',ar') -> (ident_eq v v' && ident_eq m m' && Array.length ar' == nargs)
    | LACSet s -> List.exists
                    (function
                     | LPattern (m',v',ar') -> (ident_eq v v' &&
                                                  ident_eq m m' &&
                                                    Array.length ar' == nargs)
                     | _ -> false)
                    s
    | _ -> false

type case =
  | CConst   of int*ident*ident*bool
  | CDB      of int*int
  | CLam
  | Unflatten of int

type arg_pos = { position:int; depth:int }
type abstract_problem = arg_pos * int LList.t


type matching_problem =
  | Syntactic of arg_pos LList.t
  | MillerPattern of abstract_problem LList.t

let pp_matching_problem fmt matching_problem =
  match matching_problem with
  | Syntactic     _ -> fprintf fmt "Sy"
  | MillerPattern _ -> fprintf fmt "Mi"


type dtree =
  | Fetch   of int * case * dtree * dtree option
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of matching_problem * constr list * term * dtree option


let rec pp_dtree t fmt dtree =
  let tab = String.make (t*4) ' ' in
  match dtree with
  | Test (mp,[],te,None)   -> fprintf fmt "(%a) %a" pp_matching_problem mp pp_term te
  | Test (mp,[],te,def)      ->
     fprintf fmt "\n%sif true then (%a) %a\n%selse (%a) %a"
             tab pp_matching_problem mp pp_term te tab pp_matching_problem mp (pp_def (t+1)) def
  | Test (mp,lst,te,def)  ->
     let aux out = function
       | Linearity (i,j) -> fprintf out "%d =l %d" i j
       | Bracket (i,j) -> fprintf out "%a =b %a" pp_term (mk_DB dloc dmark i) pp_term j
     in
     fprintf fmt "\n%sif %a then (%a) %a\n%selse (%a) %a" tab (pp_list " and " aux) lst
             pp_matching_problem mp pp_term te tab pp_matching_problem mp (pp_def (t+1)) def
  | Fetch (i,case,tree_suc,tree_def) ->
     (match case with
       | Unflatten n ->
          fprintf fmt "\n%sif $%i is an AC symbol applied to exactly %i args then %a\n%selse %a"
                  tab i n
       | CConst (nargs,m,v,false) ->
          fprintf fmt "\n%sif $%i is AC applied to %a.%a (%i args) then %a\n%selse %a"
                  tab i pp_ident m pp_ident v nargs
       | CConst (nargs,m,v,true) ->
          fprintf fmt "\n%sif $%i is AC applied to %a.%a (%i args, first 2 AC flattened) then %a\n%selse %a"
                  tab i pp_ident m pp_ident v nargs
       | CDB (nargs,n) ->
          fprintf fmt "\n%sif $%i is AC applied to DB[%i] (%i args) then %a\n%selse %a"
                  tab i n nargs
       | CLam -> fprintf fmt "\n%sif $%i is AC applied to Lambda then %a\n%selse %a" tab i
     ) (pp_dtree (t+1)) tree_suc tab (pp_def (t+1)) tree_def
  | Switch (i,cases,def) ->
     let pp_case out = function
       | Unflatten n, g -> assert false
       | CConst (nargs,m,v,false), g ->
          fprintf out "\n%sif $%i=%a.%a (%i args) then %a"
                  tab i pp_ident m pp_ident v nargs (pp_dtree (t+1)) g
       | CConst (nargs,m,v,true), g ->
          fprintf out "\n%sif $%i=%a.%a (%i args, first 2 AC flattened) then %a"
                  tab i pp_ident m pp_ident v nargs (pp_dtree (t+1)) g
       | CDB (nargs,n), g ->
          fprintf out "\n%sif $%i=DB[%i] (%i args) then %a"
                  tab i n nargs (pp_dtree (t+1)) g
       | CLam, g -> fprintf out "\n%sif $%i=Lambda then %a" tab i (pp_dtree (t+1)) g
     in
     fprintf fmt "%a\n%sdefault: %a" (pp_list "" pp_case)
             cases tab (pp_def (t+1)) def
and pp_case fmt = function
  | Unflatten n -> fprintf fmt "Unflatten %i" n
  | CConst (nargs,m,v,false) ->
     fprintf fmt "CConst %a.%a (%i args)" pp_ident m pp_ident v nargs
  | CConst (nargs,m,v,true) ->
     fprintf fmt "AC CConst %a.%a (%i args)" pp_ident m pp_ident v nargs
  | CDB (nargs,n) -> fprintf fmt "DB[%i] (%i args)" n nargs
  | CLam -> fprintf fmt "Lambda"


and pp_def t fmt def =
  match def with
  | None   -> fprintf fmt "FAIL"
  | Some g -> pp_dtree t fmt g

let pp_dtree fmt dtree = pp_dtree 0 fmt dtree

type rw = ident * ident * dtree

let pp_rw fmt (m,v,g) =
  fprintf fmt "GDT for '%a.%a' : %a"
    pp_ident m pp_ident v pp_dtree g

let eq a b =
  match a, b with
    | CLam              , CLam                   -> true
    | CDB    (ar,n)     , CDB    (ar',n')        -> (ar==ar' && n==n')
    | CConst (ar,m,v,ac), CConst (ar',m',v',ac') -> (ar==ar' && ident_eq v v' && ident_eq m m')
    | _, _ -> false


let case_of_pattern (is_AC:ident->ident->bool) : wf_pattern -> case option = function
  | LVar _ | LJoker      -> None
  | LPattern (m,v,pats)  -> Some (CConst (Array.length pats,m,v,
                                        is_AC m v && Array.length pats >= 2))
  | LBoundVar (_,n,pats) -> Some (CDB (Array.length pats,n))
  | LLambda _            -> Some CLam
  | LACSet _ -> assert false

let case_pattern_match (case:case) (pat:wf_pattern) : bool = match case, pat with
  | CConst (lpats,m',v',_), LPattern  (m,v,pats) -> ident_eq m m' && ident_eq v v' &&
                                                      lpats == Array.length pats
  | CDB    (lpats,n')     , LBoundVar (_,n,pats) -> n' == n && lpats == Array.length pats
  | CLam                  , LLambda _            -> true
  | _ -> false


let flatten_pattern (m:ident) (v:ident) pats =
  let rec flatten acc = function
  | [] -> acc
  | LPattern(m',v',args)::tl when ident_eq m m' && ident_eq v v' && Array.length args == 2 ->
     flatten acc (args.(0) :: args.(1) :: tl)
  | t :: tl -> flatten (t::acc) tl
  in
  let pats = flatten [] pats in
  pats


let specialize_AC_rule case (c:int) (nargs:int) (r:dtree_rule) : dtree_rule =
  let size = Array.length r.pats in
  let new_pats_c, pat =
    match r.pats.(c), case with
    | LACSet l, Unflatten _ -> LJoker, LACSet l
    | LACSet l, c ->
       let rec remove_case acc = function
         | [] -> assert false
         | hd :: tl -> if case_pattern_match case hd
                       then LACSet(List.rev_append acc tl), hd
                       else remove_case (hd::acc) tl
       in
       remove_case [] l
    | _ -> assert false in
  let aux i =
    if i < size then
      if i==c then new_pats_c else r.pats.(i)
    else (* size <= i < size+nargs *)
      match pat, case with
      | LACSet l, Unflatten n -> (assert (nargs==n);
                                  assert (n==List.length l);
                                  List.nth l (i - size))
      | LPattern (m,v,pats2), CConst(nargs',m',v',true) ->
         assert(ident_eq m m'); assert(ident_eq v v');
         assert(nargs >= 1);
         assert(Array.length pats2 == (nargs+1) );
         if i == size
         then LACSet (flatten_pattern m v [pats2.(0);pats2.(1)])
         else pats2.(i - size + 1)
      | LPattern (_,id,pats2),_ | LBoundVar (id,_,pats2),_ ->
         assert ( Array.length pats2 == nargs );
         pats2.(i - size)
      | LLambda (_,p),_ -> ( assert ( nargs == 1); p )
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
        | LPattern (m,v,pats2), CConst(nargs',m',v',true) ->
           assert(ident_eq m m'); assert(ident_eq v v');
           assert(nargs >= 1);
           assert(Array.length pats2 == (nargs+1) );
           if i == size
           then LACSet (flatten_pattern m v [pats2.(0);pats2.(1)])
           else pats2.(i - size + 1)
        | LACSet _, _ -> assert false
        | LPattern (_,id,pats2),_ | LBoundVar (id,_,pats2),_ ->
           assert ( Array.length pats2 == nargs );
           pats2.( i - size)
        | LLambda (_,p),_ -> ( assert ( nargs == 1); p )
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
let specialize_AC (mx:matrix) (c:int) (case:case) : matrix * matrix option =
  let nargs, (rules_suc, rules_def) = match case with
    | CLam                   -> 1    , List.partition (filter_on_lambda c)
                                                      (mx.first::mx.others)
    | CDB      (nargs,n)     -> nargs, List.partition (filter_on_bound_variable c nargs n)
                                                      (mx.first::mx.others)
    | CConst   (nargs,m,v,_) -> nargs, List.partition (filter_on_pattern c nargs m v)
                                                      (mx.first::mx.others)
    | Unflatten n            -> n    , List.partition (filter_on_variable_AC_set n c)
                                                      (mx.first::mx.others)
  in
  let ac_const = match case with CConst(_,_,_,t) -> t | _ -> false in
  let add_args = if ac_const then nargs-1 else nargs in
  let new_cn = match case with
    | CLam                -> spec_col_depth_l c          mx.col_depth
    | _                   -> spec_col_depth   c add_args mx.col_depth
  in
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
  let (mx_opt,nargs) = match case with
    | CLam                   -> ( filter (filter_on_lambda         c          ) mx , 1    )
    | CDB      (nargs,n)     -> ( filter (filter_on_bound_variable c nargs n  ) mx , nargs)
    | CConst   (nargs,m,v,_) -> ( filter (filter_on_pattern        c nargs m v) mx , nargs)
    | Unflatten n -> assert false
  in
  let ac_const = match case with CConst(_,_,_,t) -> t | _ -> false in
  let add_args = if ac_const then nargs-1 else nargs in
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



let partition_AC (is_AC:ident->ident->bool) (l:wf_pattern list) : case =
  let rec f x = function
    | [] -> Unflatten x
    | hd::tl ->
       begin
         match case_of_pattern is_AC hd with
         | Some c -> c
         | None   -> f (x+1) tl
       end in
  f 0 l
  
let partition (is_AC:ident->ident->bool) (mx:matrix) (c:int) : case list =
  let aux lst li =
    match case_of_pattern is_AC li.pats.(c) with
    | Some c -> if List.exists (eq c) lst then lst else c::lst
    | None   -> lst
  in
  List.fold_left aux [] (mx.first::mx.others)

(* ***************************** *)

let array_to_llist arr =
  LList.make_unsafe (Array.length arr) (Array.to_list arr)

let get_first_term mx = mx.first.right
let get_first_constraints mx = mx.first.constraints

(* Extracts the matching_problem from the first line. *)
let get_first_matching_problem mx =
  let esize = mx.first.esize in
  let dummy = { position=(-1); depth=0; } in
  let dummy2 = dummy, LList.nil in
  let arr1 = Array.make esize dummy in
  let arr2 = Array.make esize dummy2 in
  let mp = ref false in
  Array.iteri
    (fun i p ->
      match p with
      | LJoker -> ()
      | LVar (_,n,lst) ->
         begin
           let depth = mx.col_depth.(i) in
           let n = n - depth in
           assert( 0 <= n );
           assert(n < esize );
           let pos = { position=i; depth = mx.col_depth.(i) } in
           arr1.(n) <- pos;
           arr2.(n) <- pos, LList.of_list lst;
           mp := !mp || (lst <> []);
         end
      | _ -> assert false
    ) mx.first.pats ;
  ( Array.iter ( fun r -> assert (r.position >= 0 ) ) arr1 );
  if !mp then MillerPattern (array_to_llist arr2)
    else Syntactic (array_to_llist arr1)

(******************************************************************************)

(* Give the index of the first non variable column *)
let choose_column mx =
  let rec aux i =
    if i < Array.length mx.first.pats then
      let pat = mx.first.pats.(i) in
      ( match pat with
        | LPattern _ | LLambda _ | LBoundVar _ | LACSet _ -> Some i
        | LVar _ | LJoker -> aux (i+1))
    else None
  in
  aux 0

(* Construct a decision tree out of a matrix *)
let rec to_dtree (is_AC:ident->ident->bool) (mx:matrix) : dtree =
  match choose_column mx with
    (* There are only variables on the first line of the matrix *)
    | None   -> Test ( get_first_matching_problem mx,
                       get_first_constraints mx,
                       get_first_term mx,
                       map_opt (to_dtree is_AC) (pop mx) )
    (* Pattern on the first line at column c *)
    | Some c ->
       match mx.first.pats.(c) with
       | LACSet l ->
          let case = partition_AC is_AC l in
          let mx_suc, mx_def = specialize_AC mx c case in
          Fetch (c, case, to_dtree is_AC mx_suc, map_opt (to_dtree is_AC) mx_def)
       | _ ->
          let cases = partition is_AC mx c in
          let aux ca = ( ca , to_dtree is_AC (specialize mx c ca) ) in
          Switch (c, List.map aux cases, map_opt (to_dtree is_AC) (filter_default mx c) )

(******************************************************************************)


let of_rules (is_AC:ident->ident->bool) (rs:rule_infos list) : (dtree,dtree_error) error =
  try
    let r1, ro = match rs with
      | [] -> assert false
      | r1::ro -> r1,ro
    in
    (*  Building a matrix out of the non-empty list of rules r1::ro
     *  It is checked that all rules have the same head symbol and arity.
     *)
    let o = List.map (
                fun r2 ->
                if not (ident_eq r1.id r2.id) then
                  raise (DtreeExn (HeadSymbolMismatch (r2.l,r2.id,r1.id)))
                else
                  to_dtree_rule r2
              ) ro
    in
    let mx = { first=(to_dtree_rule r1); others=o; col_depth=Array.make 1 0 ;} in
    OK(to_dtree is_AC mx)
  with DtreeExn e -> Err e
