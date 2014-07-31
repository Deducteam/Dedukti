open Types

(* ******************* Matrix Definition ********************** *)

type pattern2 =
  | Joker2
  | Var2         of ident*int*int array
  | Lambda2      of ident*pattern2
  | Pattern2     of ident*ident*pattern2 array
  | BoundVar2    of ident*int*pattern2 array

type rule2 =
    { loc:loc ; pats:pattern2 array ; right:term ;
      constraints:(term*term) list ; env_size:int ; }

type matrix = { col_nums:int array; stack_size:int;
                first:rule2 ; others:rule2 list ; }

let mk_matrix (f:rule2) (o:rule2 list) : matrix =
  assert (List.for_all (fun r -> Array.length f.pats == Array.length r.pats) o);
  { first=f; others=o;
    stack_size=Array.length f.pats;
    col_nums=Array.init (Array.length f.pats) (fun i -> i) ;}

let matrix_from_others mx =
  match mx.others with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

let mx_filter (f:rule2 -> bool) (mx:matrix) : matrix option =
  match List.filter f (mx.first::mx.others) with
    | [] -> None
    | f::o -> Some { mx with first=f; others=o; }

(* Remove column [c] in [line] and append [args] *)
(* Specialize the matrix [mx] on column [c] ie remove colum [c]
 * and append [nargs] new column at the end.
 * These new columns contain
 * if we remove a pattern in column [c], the arguments;
 * if we remove a lambda in column [c], its body;
 * otherwise Jokers
* *)
let mx_specialize (mx:matrix) (c:int) (nargs:int) : matrix =
  let old_size = Array.length mx.first.pats in
  let new_size = old_size - 1 + nargs in
    assert ( old_size > 0 && c < old_size && new_size >= 0 );
  let init pats i =
    if i<c then pats.(i)
    else if i<(old_size-1) then pats.(i+1)
    else (* old_size -1 <= i < new_size *)
      match pats.(c) with
        | Joker2 | Var2 _ -> Joker2
        | Pattern2 (_,_,pats2) | BoundVar2 (_,_,pats2) ->
            ( assert ( Array.length pats2 == nargs );
              pats2.( i - old_size + 1 ) )
        | Lambda2 (_,p) ->
            ( assert ( i - old_size + 1 == 0); (* ie nargs=1 *) p )
  in
  let aux r =
    { r with pats = Array.init new_size (init r.pats) }
  in
  let new_cn = Array.init new_size (
    fun i ->
      if i<c then mx.col_nums.(i)
      else if i<(old_size-1) then mx.col_nums.(i+1)
      else (* old_size -1 <= i < new_size *)
        mx.stack_size + ( i - (old_size - 1) )
  ) in
    { first=aux mx.first; others=List.map aux mx.others;
      col_nums=new_cn; stack_size=mx.stack_size+nargs; }

(* Selects all the lines matching anything on column [c] *)
let mx_default (mx:matrix) (c:int) : matrix option =
  mx_filter (
    fun r -> match r.pats.(c) with
      | Var2 _ | Joker2 -> true
      | Lambda2 _ | Pattern2 _ | BoundVar2 _ -> false
  ) mx

(* ******************* From rules to matrix ********************** *)

(* The goal of this transformation make the patterns linear and to record the
 * non-linearity and conditionnal rewriting constraints. *)

let br = hstring "{_}"

let extract_bv k = function
  | Var (_,_,n,[]) when n<k -> n
  | _ -> assert false (*FIXME error msg*)


let is_closed k t =
  let rec aux q = function
  | Kind | Type _ | Const _ -> true
  | DB (_,_,n) -> ( n<q || n>= (k+q) )
  | Lam (_,_,a,b) | Pi (_,_,a,b) -> (aux q a) && (aux (q+1) b)
  | App (f,a,args) -> List.for_all (aux q) (f::a::args)
  in
    aux 0 t

let to_rule2 (r:rule) : rule2 =
  (*FIXME not really functionnal... *)
  let esize = List.length r.ctx in (*FIXME*)
  let fresh_var = ref esize in
  let get_fvar () = let x = !fresh_var in incr fresh_var ; x in
  let constraints = ref [] in
  let seen = Array.create esize false in
  let rec linearize k = function
    | Lambda (l,x,p) -> Lambda2 (x,linearize (k+1) p)
    | Var (l,x,n,args) when n<k ->
        BoundVar2 (x,n,Array.of_list (List.map (linearize k) args))
    | Var (l,x,n,args) (* n>=k *) ->
        let args2 = Array.of_list (List.map (extract_bv k) args) in
          if seen.(n) then
            begin
              let fvar = get_fvar () in
                constraints := (mk_DB l x fvar,mk_DB l x n)::(!constraints) ;
                Var2(x,fvar,args2)
            end
          else ( seen.(n) <- true ; Var2(x,n,args2) )
    | Brackets t ->
        let fvar = get_fvar () in
          if is_closed k t then
            begin
              constraints := (mk_DB dloc br fvar,t)::(!constraints) ;
              Var2(br,fvar,[||])
            end
          else assert false (*FIXME err msg*)
    | Pattern (_,m,v,args) ->
        Pattern2(m,v,Array.of_list (List.map (linearize k) args))
    | Joker _ -> Joker2
  in
  let args = List.map (linearize 0) r.args in
    { loc=r.l ; pats=Array.of_list args ; right=r.rhs ;
      constraints= !constraints ; env_size= esize ; }

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
        ) rs in
        mk_matrix r1' rs'

(* ******************* Matrix manipulation ********************** *)

let eq a b =
  match a, b with
    | CLam, CLam -> true
    | CDB (_,n), CDB (_,n') when n==n' -> true
    | CConst (_,m,v), CConst (_,m',v') when (ident_eq v v' && ident_eq m m') -> true
    | _, _ -> false

let force = function None -> assert false | Some x -> x
let filter_l c r =
  match r.pats.(c) with Lambda2 _ -> true | _ -> false
let filter_bv c n r =
  match r.pats.(c) with BoundVar2 (_,m,_) when n==m -> true | _ -> false
let filter_p c m v r =
  match r.pats.(c) with
    | Pattern2 (m',v',_) when ident_eq v v' && ident_eq m m' -> true
    | _ -> false

(* Partition the matrix [mx] into a new matrix specialized for each
* head symbol of pattern in column [c] *)
let partition (c:int) (mx:matrix) : (case*matrix) list =
  let aux lst li =
    let add h = if List.exists (eq h) lst then lst else h::lst in
      match li.pats.(c) with
        | Pattern2 (m,v,pats)  -> add (CConst (Array.length pats,m,v))
        | BoundVar2 (_,n,pats) -> add (CDB (Array.length pats,n))
        | Lambda2 _ -> add CLam
        | Var2 _ | Joker2 -> lst
  in
  let plst = List.fold_left aux [] (mx.first::mx.others) in
  let specialize = function
    | CLam -> ( CLam , mx_specialize (force (mx_filter (filter_l c) mx)) c 1 )
    | CDB (nargs,n) ->
        ( CDB ( nargs , n) ,
          mx_specialize (force (mx_filter (filter_bv c n) mx)) c nargs )
    | CConst (nargs,m,v) ->
        ( CConst (nargs,m,v) ,
          mx_specialize (force (mx_filter (filter_p c m v) mx)) c nargs)
  in
    List.map specialize plst (*FIXME rev_map*)

(* ******************* From matrix to dtree ********************** *)

let get_mtch_pbs (n:int) (line:pattern2 array) (col_nums:int array) : mtch_pb list =
  let arr = Array.create n (-1,[]) in (*FIXME verifier les -1 a la fin*)
    Array.iteri
      (fun i p -> match p with
         | Var2 (_,n,l) -> arr.(n) <- (col_nums.(i),Array.to_list l)
         | Joker2 -> () | _ -> assert false
      ) line ;
    Array.to_list arr

(* Give the index of the first non variable column *)
let choose_col (r:rule2) : int option =
  let rec aux i =
    if i < Array.length r.pats then
      ( match r.pats.(i) with
        | Pattern2 _ | Lambda2 _ | BoundVar2 _ -> Some i
        | Var2 _ | Joker2 -> aux (i+1) )
    else None
  in aux 0

(* Construct a decision tree out of a matrix *)
let rec to_dtree (mx:matrix) : dtree =
    match choose_col mx.first with
      | None   ->
          (* Only variables on the first line of the matrix *)
          (*TODO utiliser Syntactic *)
          Test ( MillerPattern (get_mtch_pbs mx.first.env_size mx.first.pats mx.col_nums),
                 mx.first.constraints,
                 mx.first.right,
                 map_opt to_dtree (matrix_from_others mx) )
      | Some c ->
          (* Pattern on the first line at column c *)
          let mx_cases = partition c mx in
            Switch (c,
                    List.rev_map (fun (c,mx) -> (c, to_dtree mx)) mx_cases,
                    map_opt to_dtree (mx_default mx c) )

(* ******************* Entry ********************** *)

let add_rules (rwi:rw_infos) (rs:rule list) : rw_infos =
  assert (rs != [] ) ;
  let ( ty , rules ) = match rwi with
    | Decl ty                   -> ( ty , rs )
    | Decl_rw (ty,rs0,_,_)       -> ( ty , rs0@rs )
    | Def (_,_)                 ->
        let r = match rs with r::_ -> r | _ -> assert false in
          Global.fail r.l "Cannot add rewrite\
            rules for the defined symbol '%a'." pp_ident r.id
  in
  let mx = matrix_of_rules rules in
    Decl_rw ( ty, rules , Array.length mx.first.pats , to_dtree mx )
