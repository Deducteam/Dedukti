open Types

(*TODO check comments*)

type pattern2 =
  | Joker2
  | Var2         of ident*int*int list
  | Lambda2      of ident*pattern2
  | Pattern2     of ident*ident*pattern2 array
  | BoundVar2    of ident*int*pattern2 array

type rule2 =
    { loc:loc ; pats:pattern2 array ; right:term ;
      constraints:(term*term) list ; esize:int ; }

(* ***************************** *)

let fold_map (f:'b->'a->('c*'b)) (b0:'b) (alst:'a list) : ('c list*'b) =
  let (clst,b2) =
    List.fold_left (fun (accu,b1) a -> let (c,b2) = f b0 a in (c::accu,b2))
      ([],b0) alst in
    ( List.rev clst , b2 )

(* ***************************** *)

module IntSet = Set.Make(struct type t=int let compare=(-) end)
type lin_ty = { cstr:(term*term) list; fvar:int ; seen:IntSet.t }
let br = hstring "{_}"

(* This function extracts non-linearity and bracket constraints from a list
 * of patterns. *)
let rec linearize (esize:int) (lst:pattern list) : pattern2 list * (term*term) list =
  let rec aux k (s:lin_ty) = function
  | Lambda (l,x,p) ->
      let (p2,s2) = (aux (k+1) s p) in
        ( Lambda2 (x,p2) , s2 )
  | BoundVar (l,x,n,args) ->
      let (args2,s2) = fold_map (aux k) s args in
        ( BoundVar2 (x,n,Array.of_list args2) , s2 )
  | MatchingVar (l,x,n,args) (* n>=k *) ->
      let args2 = List.map (fun (_,_,z) -> z) args in
        if IntSet.mem n (s.seen) then
          ( Var2(x,s.fvar,args2) ,
            { s with fvar=(s.fvar+1);
                     cstr= (mk_DB l x s.fvar,mk_DB l x n)::(s.cstr) ; } )
        else
          ( Var2(x,n,args2) , { s with seen=IntSet.add n s.seen; } )
    | Brackets t ->
        ( Var2(br,s.fvar,[]), {s with fvar=(s.fvar+1);
                                      cstr=(mk_DB dloc br s.fvar,t)::(s.cstr) ;} )
    | Pattern (_,m,v,args) ->
        let (args2,s2) = (fold_map (aux k) s args) in
          ( Pattern2(m,v,Array.of_list args2) , s2 )
    | Joker _ -> ( Joker2 , s )
  in
  let fm = fold_map (aux 0) { fvar=esize; cstr=[]; seen=IntSet.empty; } lst in
    ( fst fm , (snd fm).cstr )

let to_rule2 (r:rule) : rule2 =
  let esize = List.length r.ctx in
  let (pats2,cstr) = linearize esize r.args in
    { loc=r.l ; pats=Array.of_list pats2 ; right=r.rhs ;
      constraints=cstr ; esize=esize ; }

(* ***************************** *)

type matrix =
    { col_depth: int array;
      first:rule2 ;
      others:rule2 list ; }

(*
 * col_nums:    [ (n_0,p_0)     (n_1,p_1)       ...     (n_k,p_k) ]
 * first:       [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 * others:      [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 *                      ...
 *              [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 *
 *              n_i is a pointeur to the stack
 *              k_i records the depth of the column (number of binders under which it stands)
 *)

(* mk_matrix lst builds a matrix out of the non-empty list of rules [lst]
*  It is checked that all rules have the same head symbol and arity.
* *)
let mk_matrix = function
  | [] -> assert false
  | r1::rs ->
      let f = to_rule2 r1 in
      let n = Array.length f.pats in
      let o = List.map (
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
      ) rs in
        { first=f; others=o; col_depth=Array.make (Array.length f.pats) 0 ;}

let pop mx =
  match mx.others with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

let width mx = Array.length mx.first.pats

let get_first_term mx = mx.first.right
let get_first_constraints mx = mx.first.constraints

(* ***************************** *)

let filter (f:rule2 -> bool) (mx:matrix) : matrix option =
  match List.filter f (mx.first::mx.others) with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

(* Keeps only the rules with a lambda on column [c] *)
let filter_l c r =
  match r.pats.(c) with
    | Lambda2 _ | Joker2 | Var2 _ -> true
    | _ -> false

(* Keeps only the rules with a bound variable of index [n] on column [c] *)
let filter_bv c n r =
  match r.pats.(c) with
    | BoundVar2 (_,m,_) when n==m -> true
    | Var2 _ | Joker2 -> true
    | _ -> false

(* Keeps only the rules with a pattern head by [m].[v] on column [c] *)
let filter_p c m v r =
  match r.pats.(c) with
    | Pattern2 (m',v',_) when ident_eq v v' && ident_eq m m' -> true
    | Var2 _ | Joker2 -> true
    | _ -> false

(* Keeps only the rules with a joker or a variable on column [c] *)
let default (mx:matrix) (c:int) : matrix option =
  filter (
    fun r -> match r.pats.(c) with
      | Var2 _ | Joker2 -> true
      | Lambda2 _ | Pattern2 _ | BoundVar2 _ -> false
  ) mx

(* ***************************** *)

(* Specialize the rule [r] on column [c]
 * i.e. remove colum [c] and append [nargs] new column at the end.
 * These new columns contain
 * - the arguments if column [c] is a the pattern
 * - or the body if column [c] is a lambda
 * - or Jokers otherwise
* *)
let specialize_rule (c:int) (nargs:int) (r:rule2) : rule2 =
  let size = Array.length r.pats in
  let aux i =
    if i < size then
      if i==c then
        match r.pats.(c) with
          | Var2 _ as v -> v
          | _ -> Joker2
      else r.pats.(i)
    else (* size <= i < size+nargs *)
      match r.pats.(c) with
        | Joker2 | Var2 _ -> Joker2
        | Pattern2 (_,_,pats2) | BoundVar2 (_,_,pats2) ->
            ( assert ( Array.length pats2 == nargs );
              pats2.( i - size ) )
        | Lambda2 (_,p) -> ( assert ( nargs == 1); p )
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
    Array.init size aux

(* Specialize the matrix [mx] on column [c] *)
let specialize mx c case =
  let (mx_opt,nargs) = match case with
    | CLam -> ( filter (filter_l c) mx , 1 )
    | CDB (nargs,n) -> ( filter (filter_bv c n) mx , nargs )
    | CConst (nargs,m,v) -> ( filter (filter_p c m v) mx , nargs )
  in
  let new_cn = match case with
    | CLam -> spec_col_depth_l c mx.col_depth
    | _ ->    spec_col_depth c nargs mx.col_depth
  in
    match mx_opt with
      | None -> assert false
      | Some mx2 ->
          { first = specialize_rule c nargs mx2.first;
            others = List.map (specialize_rule c nargs) mx2.others;
            col_depth = new_cn;
          }

(* ***************************** *)

(* Give the index of the first non variable column *)
let choose_column mx =
  let rec aux i =
    if i < Array.length mx.first.pats then
      ( match mx.first.pats.(i) with
        | Pattern2 _ | Lambda2 _ | BoundVar2 _ -> Some i
        | Var2 _ | Joker2 -> aux (i+1) )
    else None
  in aux 0

let eq a b =
  match a, b with
    | CLam, CLam -> true
    | CDB (_,n), CDB (_,n') when n==n' -> true
    | CConst (_,m,v), CConst (_,m',v') when (ident_eq v v' && ident_eq m m') -> true
    | _, _ -> false

let partition mx c =
  let aux lst li =
    let add h = if List.exists (eq h) lst then lst else h::lst in
      match li.pats.(c) with
        | Pattern2 (m,v,pats)  -> add (CConst (Array.length pats,m,v))
        | BoundVar2 (_,n,pats) -> add (CDB (Array.length pats,n))
        | Lambda2 _ -> add CLam
        | Var2 _ | Joker2 -> lst
  in
    List.fold_left aux [] (mx.first::mx.others)

(* ***************************** *)

let array_to_llist arr =
  LList.make_unsafe (Array.length arr) (Array.to_list arr)
(*
let pp_pattern2 out = function
  | Joker2 -> Printf.fprintf out "Joker"
  | Var2 (x,i,[]) -> Printf.fprintf out "%a[%i]" pp_ident x i
  | _ -> assert false
let dump_pat_arr arr =
  Global.debug_no_loc 1 " ================ PATS >";
  Array.iter (fun p -> Global.debug_no_loc 1 "%a" pp_pattern2 p) arr ;
  Global.debug_no_loc 1 " < ================"
 *)

(* Extracts the pre_context from the first line. *)
let get_first_pre_context mx =
  let esize = mx.first.esize in
  let arr1 = Array.create esize (-1) in
  let arr2 = Array.create esize (-1,[]) in
    Array.iteri
      (fun i p -> match p with
         | Joker2 -> ()
         | Var2 (_,n,lst) ->
               begin
                 let k = mx.col_depth.(i) in
                 assert( 0 <= n-k && n-k < esize ) ;
                 arr1.(n-k) <- i;
                 arr2.(n-k) <- (i,lst)
               end
         | _ -> assert false
      ) mx.first.pats ;
    let check = ref true in
      Array.iteri
        (fun i -> function
           | (_,_::_) -> check := false
           | (_,_) -> ()
        ) arr2 ;
      if !check then Syntactic (array_to_llist arr1)
      else MillerPattern (array_to_llist arr2)
