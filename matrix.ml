open Types

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

let br = hstring "{_}"

let extract_bv k = function (*TODO move in scoping ?*)
  | Var (_,_,n,[]) when n<k -> n
  | _ -> assert false (*TODO error msg*)

let is_closed k t =
  let rec aux q = function
  | Kind | Type _ | Const _ -> true
  | DB (_,_,n) -> ( n<q || n>= (k+q) )
  | Lam (_,_,a,b) | Pi (_,_,a,b) -> (aux q a) && (aux (q+1) b)
  | App (f,a,args) -> List.for_all (aux q) (f::a::args)
  in
    aux 0 t

module IntSet = Set.Make(struct type t=int let compare=(-) end)
type lin_ty = { cstr:(term*term) list; fvar:int ; seen:IntSet.t }

let rec linearize (esize:int) (lst:pattern list) : pattern2 list * (term*term) list =
  let rec aux k s = function
  | Lambda (l,x,p) ->
      let (p2,s2) = (aux (k+1) s p) in
        ( Lambda2 (x,p2) , s2 )
  | Var (l,x,n,args) when n<k ->
      let (args2,s2) = fold_map (aux k) s args in
        ( BoundVar2 (x,n,Array.of_list args2) , s2 )
  | Var (l,x,n,args) (* n>=k *) ->
      let args2 = List.map (extract_bv k) args in
          if IntSet.mem n (s.seen) then
            ( Var2(x,s.fvar,args2) ,
              { s with fvar=(s.fvar+1);
                       cstr= (mk_DB l x s.fvar,mk_DB l x n)::(s.cstr) ; } )
          else
            ( Var2(x,n,args2) , { s with seen=IntSet.add n s.seen; } )
    | Brackets t ->
          if is_closed k t then (*TODO move in scoping ?*)
            ( Var2(br,s.fvar,[]) ,
              { s with fvar=(s.fvar+1);
                       cstr=(mk_DB dloc br s.fvar,t)::(s.cstr) ; } )
          else
              Global.fail (get_loc t) "terms between brackets should be closed."
    | Pattern (_,m,v,args) ->
        let (args2,s2) = (fold_map (aux k) s args) in
          ( Pattern2(m,v,Array.of_list args2) , s2 )
    | Joker _ -> ( Joker2 , s )
  in
  let fm = fold_map (aux 0) { fvar=esize; cstr=[]; seen=IntSet.empty; } lst in
    ( fst fm , (snd fm).cstr )

let to_rule2 (r:rule) : rule2 =
  let esize = List.length r.ctx in (*TODO get rid of length ?*)
  let (pats2,cstr) = linearize esize r.args in
    { loc=r.l ; pats=Array.of_list pats2 ; right=r.rhs ;
      constraints=cstr ; esize=esize ; }

(* ***************************** *)

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

type matrix =
    { col_nums: (int*int) array;
      stack_size:int;
      first:rule2 ;
      others:rule2 list ; }

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
        { first=f; others=o; stack_size=Array.length f.pats;
          col_nums=Array.init (Array.length f.pats) (fun i -> (i,0)) ;}

let pop mx =
  match mx.others with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

let width mx = Array.length mx.first.pats

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

let filter (f:rule2 -> bool) (mx:matrix) : matrix option =
  match List.filter f (mx.first::mx.others) with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

let default (mx:matrix) (c:int) : matrix option =
  filter (
    fun r -> match r.pats.(c) with
      | Var2 _ | Joker2 -> true
      | Lambda2 _ | Pattern2 _ | BoundVar2 _ -> false
  ) mx

(* ***************************** *)

let filter_l c r =
  match r.pats.(c) with Lambda2 _ -> true | _ -> false

let filter_bv c n r =
  match r.pats.(c) with BoundVar2 (_,m,_) when n==m -> true | _ -> false

let filter_p c m v r =
  match r.pats.(c) with
    | Pattern2 (m',v',_) when ident_eq v v' && ident_eq m m' -> true
    | _ -> false

let specialize_rule (c:int) (nargs:int) (r:rule2) : rule2 =
  let old_size = Array.length r.pats in
  let new_size = old_size - 1 + nargs in
  let aux i =
    if i < c then r.pats.(i)
    else if i < (old_size-1) then r.pats.(i+1)
    else (* old_size -1 <= i < new_size *)
      match r.pats.(c) with
        | Joker2 | Var2 _ -> Joker2
        | Pattern2 (_,_,pats2) | BoundVar2 (_,_,pats2) ->
            ( assert ( Array.length pats2 == nargs );
              pats2.( i - old_size + 1 ) )
        | Lambda2 (_,p) ->
            ( assert ( i - old_size + 1 == 0); (* ie nargs=1 *) p )
  in
    { r with pats = Array.init new_size aux }

let specialize_col_num (c:int) (nargs:int) (stack_size:int)
      (col_nums: (int*int) array) : (int*int) array =
  let old_size = Array.length col_nums in
  let new_size = old_size - 1 + nargs in
  let aux i =
    if i < c then col_nums.(i)
    else if i < (old_size-1) then col_nums.(i+1)
    else (* old_size -1 <= i < new_size *)
      ( stack_size + ( i - (old_size - 1) ) , snd col_nums.(c) )
  in
    Array.init new_size aux

let specialize_col_num_lambda (c:int) (stack_size:int)
      (col_nums: (int*int) array) : (int*int) array =
  let size = Array.length col_nums in
  let aux i =
    if i < c then col_nums.(i)
    else if i < (size-1) then col_nums.(i+1)
    else (* i == size -1 *) ( stack_size , (snd col_nums.(c)) + 1 )
  in
    Array.init size aux


(* Specialize the matrix [mx] on column [c] ie remove colum [c]
 * and append [nargs] new column at the end.
 * These new columns contain
 * if we remove a pattern in column [c], the arguments;
 * if we remove a lambda in column [c], its body;
 * otherwise Jokers
* *)
let specialize mx c case =
  let (mx_opt,nargs) = match case with
    | CLam -> ( filter (filter_l c) mx , 1 )
    | CDB (nargs,n) -> ( filter (filter_bv c n) mx , nargs )
    | CConst (nargs,m,v) -> ( filter (filter_p c m v) mx , nargs )
  in
  let new_cn = match case with
    | CLam -> specialize_col_num_lambda c mx.stack_size mx.col_nums
    | _ ->    specialize_col_num c nargs mx.stack_size mx.col_nums
  in
    match mx_opt with
      | None -> assert false
      | Some mx2 ->
          { first = specialize_rule c nargs mx2.first;
            others = List.map (specialize_rule c nargs) mx2.others;
            col_nums = new_cn;
            stack_size = mx2.stack_size + nargs;
          }

(* ***************************** *)

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

(* ***************************** line *)

let get_mtch_pbs (esize:int) (line:pattern2 array) (col_nums:(int*int) array) : ctx_loc =
  let arr1 = Array.create esize (-1) in
  let arr2 = Array.create esize (-1,[]) in
    Array.iteri
      (fun i p -> match p with
         | Joker2 -> ()
         | Var2 (_,n,lst) ->
               begin
                 let (c,k) = col_nums.(i) in
                 assert( 0 <= n-k && n-k < esize ) ;
                 assert( i < Array.length line ) ;
                 arr1.(n-k) <- c;
                 arr2.(n-k) <- (c,lst)
               end
         | _ -> assert false
      ) line ;
    let check = ref true in
      Array.iteri
        (fun i -> function
           | (_,_::_) -> check := false
           | (_,_) -> ()
        ) arr2 ;
      if !check then Syntactic (Array.to_list arr1)
      else MillerPattern (Array.to_list arr2)

type line =
    { l_rhs:term ; l_eqs:(term*term) list ; l_esize:int ; l_ctx:ctx_loc; }

let first mx =
  { l_rhs=mx.first.right; l_eqs=mx.first.constraints; l_esize=mx.first.esize;
    l_ctx= get_mtch_pbs mx.first.esize mx.first.pats mx.col_nums; }
