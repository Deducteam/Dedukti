open Types


(* ******************* Matrix Definition ********************** *)

type pattern2 =
  | Var2         of ident*int
  | Pattern2     of ident*ident*pattern2 array

type rule2 =
    { loc:loc ; pats:pattern2 array ; right:term ;
      constraints:(term*term) list ; env_size:int ; }

(* a matrix is a non empty list of rule2
 * The length of the array args is the same for every rule *)

type matrix = rule2 list

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
  let esize = ref (List.length r.ctx) in
  let constraints = ref [] in
  let seen = Array.create !esize false in
  let rec linearize = function
    | Var (l,x,n) ->
        if seen.(n) then
          begin
            let fresh = !esize in
              incr esize ;
              constraints := (mk_DB l x fresh,mk_DB l x n)::(!constraints) ;
              Var2(x,fresh)
          end
        else ( seen.(n) <- true ; Var2(x,n) )
    | Brackets t ->
        begin
          let fresh = !esize in
            incr esize ;
            constraints := (mk_DB dloc br fresh,t)::(!constraints) ;
            Var2(br,fresh)
        end
    | Pattern (_,m,v,args) ->
        Pattern2(m,v,Array.of_list (List.map linearize args))
    | Joker (l,_) ->
        begin
          let fresh = !esize in
            incr esize ;
            Var2(un,fresh)
        end
  in
  let args = List.map linearize r.args in
    { loc=r.l ; pats=Array.of_list args ; right=r.rhs ;
      constraints= !constraints ; env_size= !esize ; }

(* Construction of a matrix from a non empty list of rules.
 * It is checked that all the head symbol are the same and that all arities are
 * the same. *)

let mk_matrix : rule list -> int * matrix = function
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
        ( n , r1'::rs' )

(* ******************* Matrix manipulation ********************** *)

let _v = hstring "_"

let mk_var_lst inf sup =
  let rec aux i = if i<sup then (mk_DB dloc _v i) :: (aux (i+1)) else []
  in aux inf

let rec subst_q (q:int) (te:term) (u:term) =
  let rec aux k = function
    | DB (_,_,n) as t -> if n = q+k then Subst.shift k u else t
    | Type _ | Kind | Const _ | Meta _ as t -> t
    | Lam (_,x,a,b) -> mk_Lam dloc x ( aux k a ) ( aux (k+1) b )
    | Pi  (_,x,a,b) -> mk_Pi dloc  x ( aux k a ) ( aux (k+1) b )
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te

(* Remove column [c] in [line] and append [n] fresh variables.
* the right-hand side and the constraints are updated to depends
* on these new variables instead of the removed variable in column c*)
let specialize_var (c:int) (m:ident) (v:ident) (n:int) (line:rule2) (q:int) =
  let old_n = Array.length line.pats in
  let new_n = old_n + n - 1 in
(*    assert ( c < old_n ); *)
(*    assert ( 0 <= new_n ); *)
  let new_pats =
    Array.init new_n (
      fun i ->
        if i < c              then line.pats.(i)   (* [ 0 - (c-1) ] *)
        else if i < (old_n-1) then line.pats.(i+1) (* [ c - (n-2) ] *)
        else (* i < new_n *) Var2( _v , line.env_size + (i-old_n+1) )
                                        (* [ (n-1) - (n+nargs-2) ] *)
    ) in
  let u =
    match mk_var_lst line.env_size (line.env_size + n) with
      | a::args -> mk_App (mk_Const dloc m v) a args
      | [] -> mk_Const dloc m v
  in
    { line with
          pats  = new_pats ;
          constraints = List.rev_map (
            fun (t1,t2) -> ( subst_q q t1 u , subst_q q t2 u )
          ) line.constraints ;
          right = subst_q q line.right u;
          env_size = line.env_size + n ; }

(* Remove column [c] in [line] and append [args] *)
let specialize_pat (c:int) (line:rule2) (args:pattern2 array) : rule2 =
  let  n = Array.length args in
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
    { line with pats = new_pats }

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
        | Var2 (_,var)             -> (specialize_var c m v nb_args li var)::lst
    ) in
  let lst = List.fold_left aux [] mx in
    ( nb_args , m , v , List.rev lst )


(* Selects all the lines matching anything on column [c] *)
let default c mx =
  let aux lst li = match li.pats.(c) with
    | Var2 (_,_) -> li::lst
    | _         -> lst
  in
  let lst = List.fold_left aux [] mx in
    List.rev lst

let eq m v (m',v',_) = ident_eq v v' && ident_eq m m'

(* Partition the matrix [mx] into:
 * - a new matrix specialized for each head symbol of pattern in column [c];
 * - a default matrix *)
let partition (c:int) (mx:matrix) : (int*ident*ident*matrix) list * matrix =
  let aux cstr li =
    match li.pats.(c) with
      | Pattern2 (m,v,args)      ->
          if List.exists (eq m v) cstr then cstr
          else (m,v,Array.length args)::cstr
      | Var2 _ -> cstr
  in
  let hsymbs = List.fold_left aux [] mx in
  let cases  = List.map (specialize c mx) hsymbs in
  let def    = default c mx in
    ( cases , def )

(* ******************* From matrix to dtree ********************** *)

(* Permute free variable of [t] with respect with the permutation [perm] *)
let permute l (perm:int array) (t:term) : term =
  let rec aux k = function
    | App (f,a,args)             -> mk_App (aux k f) (aux k a) (List.map (aux k) args)
    | Lam (l,x,a,f)              -> mk_Lam l x (aux k a) (aux (k+1) f)
    | Pi  (l,x,a,b)              -> mk_Pi  l x (aux k a) (aux (k+1) b)
    | DB (l,x,n) when (n>=k)     ->
(*         assert (n-k < Array.length ord); *)
        let n_db = perm.(n-k) in
          if n_db = (-1) then
            Global.fail l "Free variables on \
              the right-hand side of a rule should also appear in the left-hand side."
          else mk_DB l x (n_db + k)
    | t                         -> t
  in aux 0 t

(* Extract the permutation from a line of variables. *)
let get_permutation (l:rule2) : int array =
  let perm = Array.make l.env_size (-1) in
    Array.iteri
      (fun i p -> match p with
         | Var2 (_,n)    -> perm.(n) <- i
         | _            -> assert false
      ) l.pats ;
    perm

(* Give the index of the first non variable column *)
let choose_col (line:pattern2 array) : int option =
  let rec aux i =
    if i < Array.length line then
      ( match line.(i) with
        | Pattern2 (_,_,_) -> Some i
        | Var2 (_,_) -> aux (i+1) )
    else None
  in aux 0

(* Construct a decision tree out of a matrix *)
let rec to_dtree (mx:matrix) : dtree =
  let ( f_col , tail ) = match mx with f::tl -> (f,tl) | _ -> assert false in
    match choose_col f_col.pats with
      | None      ->
          begin
            (* Only variables on the first line of the matrix *)
            let def =
              ( match tail with
                | li::_ ->
                    if f_col.constraints = [] then
                      ( Global.debug 1 li.loc "Useless rule." ; None )
                    else Some ( to_dtree tail )
                | []    -> None ) in
            let perm = get_permutation f_col in
              (*The free variables in [te] corresponds to positions in the context
              * we make them point to position on [f_col] *)
            let te = permute f_col.loc perm f_col.right in
            let aux (t1,t2) =
              ( permute f_col.loc perm t1 , permute f_col.loc perm t2 ) in
              Test ( List.rev_map aux f_col.constraints , te , def )
          end
      | Some c    ->
          begin
            (* Pattern on the first line at column c *)
            let (mx_cases,mx_def) = partition c mx in
            let def = match mx_def with
              | []      -> None
              | _::_    -> Some (to_dtree mx_def)
            in
            let aux (n,m,v,mx) = (n,m,v,to_dtree mx) in
              Switch ( c , List.rev_map aux mx_cases , def )
          end

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
  let ( n , mx ) = mk_matrix rules in
    Decl_rw ( ty, rules , n , to_dtree mx )
