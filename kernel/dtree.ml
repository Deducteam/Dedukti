open Basic
open Term
open Rule
open Format

type Debug.flag += D_matching
let _ = Debug.register_flag D_matching "Matching"

type dtree_error =
  | HeadSymbolMismatch  of loc * name * name
  | ArityInnerMismatch  of loc * ident * ident

exception DtreeError of dtree_error

type case =
  | CConst of int * name
  | CDB    of int * int
  | CLam

type atomic_problem = { pos:int; depth:int; args_db:int LList.t }
type matching_problem = atomic_problem LList.t

type dtree =
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of Rule.rule_name * matching_problem * constr list * term * dtree option

(** Type of decision forests *)
type t = (int * dtree) list

let empty = []

(** Return first pair (ar,tree) in given list such that ar <= stack_size *)
let rec find_dtree stack_size = function
  | [] -> None
  | hd :: tl -> if fst hd <= stack_size then Some hd
    else find_dtree stack_size tl


(******************************************************************************)

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
    first    : rule_infos ;
    others   : rule_infos list ; }

(* mk_matrix lst builds a matrix out of the non-empty list of rules [lst]
*  It is checked that all rules have the same head symbol and arity.
*)
let mk_matrix (arity:int) (ri:rule_infos list) : matrix =
  let rules = List.filter (fun x -> List.length x.args <= arity) ri in
  assert (rules <> []); (* At least one rule should correspond to the given arity. *)
  let f r =
    let ar = Array.length r.pats in
    assert (ar <= arity); (* This guaranted in the of_rules function.  *)
    if ar == arity then r
    else (* Edit rule r with too low arity : add extra arguments*)
      let tail = Array.init (arity-ar) (fun i -> LVar(dmark, i + r.esize,[])) in
      let new_args =
        List.map (function LVar(x,n,[]) -> mk_DB dloc x n | _ -> assert false)
          (Array.to_list tail) in
      {r with
       esize = r.esize + arity - ar;
       rhs   = mk_App2 r.rhs new_args;
       pats  = Array.append r.pats tail
      }
  in
  let rules = List.map f rules in
  { first=List.hd rules; others=List.tl rules; col_depth=Array.make arity 0; }

(* Remove a line of the matrix [mx] and return None if the new matrix is Empty. *)
let pop mx =
  match mx.others with
  | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

let filter (f:rule_infos -> bool) (mx:matrix) : matrix option =
  match List.filter f (mx.first::mx.others) with
  | [] -> None
  | f::o -> Some { mx with first=f; others=o; }

(* Keeps only the rules with a lambda on column [c] *)
let filter_on_lambda c r =
  match r.pats.(c) with
  | LLambda _ | LJoker | LVar _ -> true
  | _ -> false

(* Keeps only the rules with a bound variable of index [n] on column [c] *)
let filter_on_bound_variable c n r =
  match r.pats.(c) with
  | LBoundVar (_,m,_) -> n == m
  | LVar _ | LJoker -> true
  | _ -> false

(* Keeps only the rules with a pattern head by [cst]
   applied to [ar] arguments on column [c] *)
let filter_on_pattern c cst ar r =
  match r.pats.(c) with
  | LPattern (cst', args) -> name_eq cst cst' && Array.length args == ar
  | LVar _ | LJoker -> true
  | _ -> false

(* Keeps only the rules with a joker or a variable on column [c] *)
let filter_default (mx:matrix) (c:int) : matrix option =
  filter (
    fun r -> match r.pats.(c) with
      | LVar _ | LJoker -> true
      | LLambda _ | LPattern _ | LBoundVar _ -> false
  ) mx

(* Specialize the rule [r] on column [c]
 * i.e. replace colum [c] with a joker and append [nargs] new column at the end.
 * These new columns contain
 * - the arguments if column [c] is a pattern
 * - or the body if column [c] is a lambda
 * - or Jokers otherwise
* *)
let specialize_rule (c:int) (nargs:int) (r:rule_infos) : rule_infos =
  let size = Array.length r.pats in
  let aux i =
    if i < size then
      if i==c then
        match r.pats.(c) with
        | LVar _ as v -> v
        | _ -> LJoker
      else r.pats.(i)
    else (* size <= i < size+nargs *)
      let check_args id pats =
        if ( Array.length pats != nargs ) then
          raise (DtreeError(ArityInnerMismatch(r.l, Basic.id r.cst, id)));
        pats.( i - size)
      in
      match r.pats.(c) with
      | LJoker | LVar _ -> LJoker
      | LPattern  (cst , pats2) -> check_args (id cst) pats2
      | LBoundVar (id,_, pats2) -> check_args id       pats2
      | LLambda (_,p) -> ( assert ( nargs == 1); p )
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

(* Specialize the matrix [mx] on column [c] *)
let specialize mx c case : matrix =
  let (mx_filter, nargs) = match case with
    | CLam               -> (filter_on_lambda c           , 1     )
    | CDB (nargs,n)      -> (filter_on_bound_variable c n , nargs )
    | CConst (nargs,cst) -> (filter_on_pattern c cst nargs, nargs ) in
  let mx_opt = filter mx_filter mx in
  let new_cn = match case with
    | CLam -> spec_col_depth_l c       mx.col_depth
    | _    -> spec_col_depth   c nargs mx.col_depth
  in
  match mx_opt with
  | None -> assert false
  | Some mx2 ->
    { first =            specialize_rule c nargs  mx2.first;
      others = List.map (specialize_rule c nargs) mx2.others;
      col_depth = new_cn;
    }


(******************************************************************************)

let partition (mx:matrix) (c:int) : case list =
  let aux lst li =
    let add h = if List.mem h lst then lst else h::lst in
    match li.pats.(c) with
    | LPattern (cst,pats)  -> add (CConst (Array.length pats,cst))
    | LBoundVar (_,n,pats) -> add (CDB (Array.length pats,n))
    | LLambda _ -> add CLam
    | LVar _ | LJoker -> lst
  in
  List.fold_left aux [] (mx.first::mx.others)


(******************************************************************************)

let get_first_term        mx = mx.first.rhs
let get_first_constraints mx = mx.first.constraints

(* Extracts the matching_problem from the first line. *)
let get_first_matching_problem mx =
  let esize = mx.first.esize in
  let dummy = { pos=(-1); depth=0; args_db=LList.nil } in
  let arr = Array.make esize dummy in
  let process i = function
    | LJoker -> ()
    | LVar (_,n,lst) ->
      begin
        let k = mx.col_depth.(i) in
        assert(0 <= n-k);
        assert(n-k < esize );
        arr.(n-k) <- { pos=i; depth=k; args_db=LList.of_list lst }
      end
    | _ -> assert false in
    Array.iteri process mx.first.pats;
    Array.iter (fun r -> assert (r.pos >= 0 )) arr;
    LList.of_array arr


(******************************************************************************)

(* Give the index of the first non variable column *)
let choose_column mx =
  let rec aux i =
    if i < Array.length mx.first.pats then
      ( match mx.first.pats.(i) with
        | LPattern _ | LLambda _ | LBoundVar _ -> Some i
        | LVar _ | LJoker -> aux (i+1) )
    else None
  in aux 0

(* Construct a decision tree out of a matrix *)
let rec to_dtree (mx:matrix) : dtree =
  match choose_column mx with
  (* There are only variables on the first line of the matrix *)
  | None   -> Test (mx.first.name,
                    get_first_matching_problem mx,
                    get_first_constraints mx,
                    get_first_term mx,
                    map_opt to_dtree (pop mx) )
  (* Pattern on the first line at column c *)
  | Some c ->
    let cases = partition mx c in
    let aux ca = ( ca , to_dtree (specialize mx c ca) ) in
    Switch (c, List.map aux cases, map_opt to_dtree (filter_default mx c) )


(******************************************************************************)

(** Adds a new arity to a (reverse) sorted list of distincts arities *)
let rec add l ar =
  match l with
  | [] -> [ar]
  | hd :: tl ->
    if ar > hd then ar :: l
    else if ar == hd then l (* ar is already in l *)
    else hd :: (add tl ar)

let of_rules = function
  | [] -> []
  | r::tl as rs ->
    let name = r.cst in
    let arities = ref [] in
    List.iter
      (fun x ->
         if not (name_eq x.cst name)
         then raise (DtreeError (HeadSymbolMismatch (x.l,x.cst,name)));
         let arity = List.length x.args in
         arities := add !arities arity)
      rs;
    let sorted_arities = List.fold_left add [] !arities in
    (* reverse sorted list of all rewrite rules arities. *)
    let aux ar =
      let m = mk_matrix ar rs in
      (ar, to_dtree m) in
    List.map aux sorted_arities


(******************************************************************************)

let pp_matching_problem fmt matching_problem = fprintf fmt "Mi"

let rec pp_dtree t fmt dtree =
  (* FIXME: Use format boxes here instead of manual tabs. *)
  let tab = String.init (1 + t*4) (fun i -> if i == 0 then '\n' else ' ') in
  match dtree with
  | Test (name,mp,[],te,None) ->
    fprintf fmt "{%a} (%a) %a" pp_rule_name name pp_matching_problem mp pp_term te
  | Test (name,mp,[],te,def)  ->
    fprintf fmt "\n%s{%a} if true then (%a) %a\n%selse (%a) %a" tab pp_rule_name name
      pp_matching_problem mp pp_term te tab pp_matching_problem mp (pp_def (t+1)) def
  | Test (name,mp,lst,te,def) ->
    let aux out = function
      | Linearity (i,j) -> fprintf out "{%a} %d =l %d" pp_rule_name name i j
      | Bracket   (i,j) -> fprintf out "{%a} %a =b %a" pp_rule_name name
                             pp_term (mk_DB dloc dmark i) pp_term j in
    fprintf fmt "\n%sif %a then (%a) %a\n%selse (%a) %a" tab (pp_list " and " aux) lst
      pp_matching_problem mp pp_term te tab pp_matching_problem mp (pp_def (t+1)) def
  | Switch (i,cases,def)->
    let pp_case out = function
      | CConst (_,cst), g ->
        fprintf out "\n%sif $%i=%a then %a" tab i pp_name cst (pp_dtree (t+1)) g
      | CLam, g -> fprintf out "\n%sif $%i=Lambda then %a" tab i (pp_dtree (t+1)) g
      | CDB (_,n), g -> fprintf out "\n%sif $%i=DB[%i] then %a" tab i n (pp_dtree (t+1)) g
    in
    fprintf fmt "%a\n%sdefault: %a" (pp_list "" pp_case)
      cases tab (pp_def (t+1)) def

and pp_def t fmt = function
  | None   -> fprintf fmt "FAIL"
  | Some g -> pp_dtree t fmt g

let pp_dtree fmt dtree = pp_dtree 0 fmt dtree

let pp_rw fmt (i,g) =
  fprintf fmt "When applied to %i argument(s): %a" i pp_dtree g

let pp_dforest fmt = function
  | []    -> fprintf fmt "No GDT.@."
  | trees -> (pp_list "\n" pp_rw) fmt trees
