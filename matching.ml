open Types

(* rules to matrix *)

type matrix = rule2 list (* never empty *)

let br = hstring "{_}"
(*
let line_from_rule (l,ctx,id,pats0,ri:rule) : line =
  let k0 = List.length ctx in
  let (k,pats,lst) = linearize k0 pats0 in
    (* match lst with | [] -> () | _ -> Global.unset_linearity l *)
    { loc = l; pats = pats; right = ri; env_size = k; constr = lst }
 *)
let rec gc_pat linearity_check env_size cstr : pattern -> int*(term*term)list*pattern =
  function
  | Var (s,n) as pat    ->
      begin
(*         assert ( n < Array.length linearity_check ) ; *)
        if linearity_check.(n) then
          ( env_size+1 , (mk_DB s n,mk_DB s env_size)::cstr , Var(s,env_size) )
        else ( linearity_check.(n) <- true ; ( env_size , cstr , pat ) )
      end
  | Pattern (m,v,args)  ->
      begin
        let dummy = Var (empty,0) in
        let args2 = Array.create (Array.length args) dummy in
        let ( _ , env_size2 , cstr2 ) =
          Array.fold_left (gc_arr linearity_check args2) (0,env_size,cstr) args in
          ( env_size2 , cstr2 , Pattern(m,v,args2) )
      end
  | Brackets t          ->
      ( env_size+1 , (mk_DB br env_size,t)::cstr , Var(br,env_size) )

and gc_arr linearity_check args2 (i,env_size,cstr) pat =
(*   assert ( i < Array.length args2 ) ; *)
  let (env_size2,cstr2,pat2) = gc_pat linearity_check env_size cstr pat in
    args2.(i) <- pat2 ; ( i+1 , env_size2 , cstr2 )

let get_constraints env_size (pats:pattern array) : int * pattern array * (term*term) list =
  let dummy = Var (empty,0) in
  let args2 = Array.create (Array.length pats) dummy in
  let linearity_check = Array.create env_size false in
  let ( _ , env_size2 , cstr ) =
    Array.fold_left (gc_arr linearity_check args2) (0,env_size,[]) pats in
    ( env_size2 , args2 , cstr )

let to_rule2 n (r:rule) : rule2 =
  if Array.length r.args = n then
    begin
      let env_size = List.length r.ctx in
      let (env_size2,args2,cstr) = get_constraints env_size r.args in
        { loc=r.l ; pats=args2 ; right=r.rhs ;
          constraints=cstr ; env_size=env_size2 ; }
    end
  else
    Global.fail r.l "All the rewrite rules for \
      the symbol '%a' should have the same arity." pp_ident r.id

let mk_matrix mx rs =
  let n = match mx, rs with
    | r::_, _   -> Array.length r.pats
    | _, r::_   -> Array.length r.args
    | _, _      -> assert false in
  let rs2 = List.map (to_rule2 n) rs in
    ( n , mx@rs2 )

(* Specialization/Partition *)

let _v = hstring "_"
let mk_var_lst inf sup =
  let rec aux i =
    if i<sup then (mk_DB _v i) :: (aux (i+1)) else []
  in aux inf

let specialize_var (c:int) (m:ident) (v:ident) (n:int) (line:rule2) (var:int) =
  let old_n = Array.length line.pats in
  let new_n = old_n + n - 1 in
(*    assert ( c < old_n ); *)
(*    assert ( 0 <= new_n ); *)
  let new_pats =
    Array.init new_n (
      fun i ->
        if i < c              then line.pats.(i)   (* [ 0 - (c-1) ] *)
        else if i < (old_n-1) then line.pats.(i+1) (* [ c - (n-2) ] *)
        else (* i < new_n *) Var( _v , line.env_size + (i-old_n+1) )
                                        (* [ (n-1) - (n+nargs-2) ] *)
    ) in
  let te =
    if n = 0 then mk_Const m v
    else mk_App ( (mk_Const m v)::
                  (mk_var_lst line.env_size (line.env_size + n) ) )
  in
    { line with
          pats  = new_pats ;
          constraints = List.rev_map (
            fun (t1,t2) -> (Subst.subst_q (var,te) 0 t1, Subst.subst_q (var,te) 0 t2 )
          ) line.constraints ;
          right = Subst.subst_q (var,te) 0 line.right ;
          env_size = line.env_size + n ; }

let specialize_pat (c:int) (line:rule2) (args:pattern array) : rule2 =
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

let specialize c mx (m,v,nb_args) =
  let aux lst li =
    ( match li.pats.(c) with
        | Pattern (m',v',args) when (ident_eq v v' && ident_eq m m') ->
            (specialize_pat c li args)::lst
        | Var (_,var)             -> (specialize_var c m v nb_args li var)::lst
        | _                       -> lst ) in
  let lst = List.fold_left aux [] mx in
    ( nb_args , m , v , List.rev lst )

let default c mx =
  let aux lst li = match li.pats.(c) with
    | Var (_,_) -> li::lst
    | _         -> lst
  in
  let lst = List.fold_left aux [] mx in
    List.rev lst

let eq m v (m',v',_) = ident_eq v v' && ident_eq m m'

let partition (c:int) (mx:matrix) : (int*ident*ident*matrix) list * matrix =
  let aux cstr li =
    match li.pats.(c) with
      | Pattern (m,v,args)      ->
          if List.exists (eq m v) cstr then cstr
          else (m,v,Array.length args)::cstr
      | _                       -> cstr
  in
  let constr = List.fold_left aux [] mx in
  let cases  = List.map (specialize c mx) constr in
  let def    = default c mx in
    ( cases , def )

(* Reordering *)

let reorder l (ord:int array) (t:term) : term =
  let rec aux k = function
    | App args                  -> mk_App (List.map (aux k) args)
    | Lam (x,a,f)               -> mk_Lam x (aux k a) (aux (k+1) f)
    | Pi  (x,a,b)               -> mk_Pi  x (aux k a) (aux (k+1) b)
    | DB (x,n) when (n>=k)      ->
(*         assert (n-k < Array.length ord); *)
        let n_db = ord.(n-k) in
          if n_db = (-1) then
            Global.fail l "Free variables on \
              the right-hand side of a rule should also appear in the left-hand side."
          else mk_DB x (n_db + k)
    | t                         -> t
  in aux 0 t

let get_order (l:rule2) : int array =
  let ord = Array.make l.env_size (-1) in
    Array.iteri
      (fun i p -> match p with
         | Var (_,n)    -> ord.(n) <- i
         | _            -> assert false
      ) l.pats ;
    ord

(* rules to dtree *)

let choose_col (tab:pattern array) : int option =
  let rec aux i =
    if i < Array.length tab then
      ( match tab.(i) with
        | Pattern (m,v,args)    -> Some i
        | _                     -> aux (i+1) )
    else None
  in aux 0

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
            let ord = get_order f_col in
            let te = reorder f_col.loc ord f_col.right in
            let aux (t1,t2) =
              ( reorder f_col.loc ord t1 , reorder f_col.loc ord t2 ) in
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

(* Entry *)

let add_rule (rwi:rw_infos) (rs:rule list) : rw_infos =
  let ( ty , mx0 ) = match rwi with
    | Decl ty                   -> ( ty , [] )
    | Decl_rw (ty,mx,_,_)       -> ( ty , mx )
    | Def (_,_)                 ->
        let r = match rs with r::_ -> r | _ -> assert false in
          Global.fail r.l "Cannot add rewrite rules for \
            the symbol '%a' since it is a defined symbol." pp_ident r.id
  in
  let ( n , mx ) = mk_matrix mx0 rs in
    Decl_rw ( ty, mx , n , to_dtree mx )
