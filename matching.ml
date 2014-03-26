open Types

(* Compilation of rewrite rules to decision tree as in:
 * Compiling Pattern Matching to Good Decision Trees (Maranget, 2008)
 * *)

type line =
    { loc:loc ; pats:pattern array ; right:term ;
      constraints:(term*term) list ; env_size:int ; }

let qm = hstring "?"

let pp_constr = Pp.pp_list " and " (
  fun out (t1,t2) -> Printf.fprintf out "%a ~ %a" Pp.pp_term t1 Pp.pp_term t2
)

let pp_line out li : unit =
  Printf.fprintf out " [ %i ] %a --> %a "
    li.env_size Pp.pp_pattern (Pattern (qm,qm,li.pats)) Pp.pp_term li.right;
  ( match li.constraints with
      | []      -> output_string out "\n"
      | _       -> Printf.fprintf out "when %a \n" pp_constr li.constraints
  ) ;
  flush out

let br = hstring "{?}"

let rec gc_pat arr k lst : pattern -> int*(term*term)list*pattern = function
  | Var (s,n) as p              ->
      if arr.(n) then ( k+1 , (mk_DB s n,mk_DB s k)::lst , Var(s,k) )
      else ( arr.(n) <- true ; ( k , lst , p ) )
  | Condition (i,t) as p        -> ( k , (mk_DB br i,t)::lst , p )
  | Pattern (m,v,args)          ->
      begin
        let dummy = Var (empty,0) in
        let args2 = Array.create (Array.length args) dummy in
        let ( _ , k2 , lst2 ) =
          Array.fold_left (gc_arr arr args2) (0,k,lst) args in
          ( k2 , lst2 , Pattern(m,v,args2) )
      end
and gc_arr arr args2 (i,k,lst) p =
  let (k2,lst2,p2) = gc_pat arr k lst p in
    args2.(i) <- p2 ; ( i+1 , k2 , lst2 )

let get_constraints env_size (pats:pattern array) : int * pattern array * (term*term) list =
  let dummy = Var (empty,0) in
  let args2 = Array.create (Array.length pats) dummy in
  let arr = Array.create env_size false in
  let ( _ , k , lst ) =
    Array.fold_left (gc_arr arr args2) (0,env_size,[]) pats in
    ( k , args2 , lst )

let line_of_rule (r:rule) : line =
  let k = List.length r.ctx in
  let (size,pats,lst) = get_constraints k r.args in
    { loc=r.l ; pats=pats ; right=r.rhs ; constraints=lst ; env_size=size ; }

let specialize_pat (c:int) (line:line) (args:pattern array) : line =
  let  n = Array.length args in
  let old_n = Array.length line.pats in
  let new_n = old_n + n - 1 in
  (* assert ( c < old_n ); *)
  (* assert ( 0 <= new_n );*)
  let new_pats =
    Array.init new_n (
      fun i ->
        if i < c              then  line.pats.(i)   (* [ 0 - (c-1) ] *)
        else if i < (old_n-1) then  line.pats.(i+1) (* [ c - (n-2) ] *)
        else (* i < new_n *) args.(i-old_n+1)       (* [ (n-1) - (n+nargs-2) ] *)
    ) in
    { line with pats = new_pats }

let _v = hstring "_"

let mk_var_lst inf sup =
  let rec aux i =
    if i<sup then (mk_DB _v i) :: (aux (i+1)) else []
  in aux inf

let specialize_var (c:int) (m:ident) (v:ident) (n:int) (line:line) (var:int) =
  let old_n = Array.length line.pats in
  let new_n = old_n + n - 1 in
  (* assert ( c < old_n ); *)
  (* assert ( 0 <= new_n );*)
  let new_pats =
    Array.init new_n (
      fun i ->
        if i < c              then  line.pats.(i)   (* [ 0 - (c-1) ] *)
        else if i < (old_n-1) then  line.pats.(i+1) (* [ c - (n-2) ] *)
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

let reorder l (ord:int array) (t:term) : term =
  let rec aux k = function
    | App args                  -> mk_App (List.map (aux k) args)
    | Lam (x,a,f)               -> mk_Lam x (aux k a) (aux (k+1) f)
    | Pi  (x,a,b)               -> mk_Pi  x (aux k a) (aux (k+1) b)
    | DB (x,n) when (n>=k)      ->
        (* assert (n-k < Array.length ord); *)
        let n_db = ord.(n-k) in
          if n_db = (-1) then
            Global.fail l "Free variables on \
              the right-hand side of a rule should also appear in the left-hand side."
          else mk_DB x (n_db + k)
    | t                         -> t
  in aux 0 t

let get_order (l:line) : int array =
  let ord = Array.make l.env_size (-1) in
    Array.iteri
      (fun i p -> match p with
         | Var (_,n) | Condition (n,_) -> ord.(n) <- i
         | Pattern _ -> assert false
      ) l.pats ;
    ord

let find_pattern (a:pattern array) =
  let rec aux i =
    if i < Array.length a then
      ( match a.(i) with
        | Pattern (m,v,args)    -> Some (i,m,v,args)
        | _                     -> aux (i+1) )
    else None
  in aux 0

let rec line_to_dtree (line:line) : dtree =
  match find_pattern line.pats with
    | None              ->
        let ord = get_order line in
        let ri = reorder line.loc ord line.right in
        let constr = List.rev_map (
          fun (t1,t2) -> (reorder line.loc ord t1,reorder line.loc ord t2)
        ) line.constraints in
          Test ( constr , ri , None )
    | Some (c,m,v,args) ->
        let case = ( Array.length args , m , v ,
                     line_to_dtree (specialize_pat c line args) ) in
          Switch ( c , [case] , None )

let rec add_line (line:line) : dtree -> dtree = function
  | Test ([],_,_) as g          -> ( Global.debug 1 line.loc "Useless rule." ; g )
  | Test (lst,te,None)          -> Test (lst,te,Some (line_to_dtree line))
  | Test (lst,te,Some g)        -> Test (lst,te,Some (add_line line g))
  | Switch (c,cases,def)        ->
      begin
        match line.pats.(c) with
          | Pattern (m,v,args)  ->
              let line2 = specialize_pat c line args in
                Switch ( c , update_cases [] m v line2 cases , def )
          | Var (_,var)
          | Condition (var,_)   ->
              let new_cases = List.map (fun x -> update_case_def c var line x) cases in
                Switch ( c , new_cases , update_def line def )
      end

and update_cases accu (m:ident) (v:ident) (line:line) = function
  | []                          ->
      List.rev ( (Array.length line.pats,m,v,line_to_dtree line)::accu )
  | ((n,m',v',tr) as hd)::tl      ->
      if ident_eq v v' && ident_eq m m' then
        List.rev_append accu ((n,m,v,add_line line tr)::tl)
      else update_cases (hd::accu) m v line tl

and update_case_def (c:int) (var:int) (line:line) (n,m,v,tr) =
  let line2 = specialize_var c m v n line var in
    match tr with
      | Test (k,t,def)          ->
          ( n , m , v , Test (k,t,update_def line2 def) )
      | Switch (k,cases,def)    ->
          ( n , m , v , Switch (k,cases,update_def line2 def) )

and update_def (line:line) : dtree option -> dtree option = function
  | None        -> None
  | Some dt     -> Some (add_line line dt)

let add_rule (rwi:rw_infos) (r:rule) : rw_infos =
  let line = line_of_rule r in
  let size = Array.length r.args in
    match rwi with
      | Decl ty                         -> Decl_rw ( ty, size , line_to_dtree line )
      | Decl_rw (ty,n,tr) when n=size   -> Decl_rw ( ty , n , add_line line tr )
      | Decl_rw (_,_,_)                 ->
          Global.fail r.l "All the rewrite rules for \
            the symbol '%a' should have the same arity." pp_ident r.id
      | Def (_,_)         ->
          Global.fail r.l "Cannot add rewrite rules for \
            the symbol '%a' since it is a defined symbol." pp_ident r.id
