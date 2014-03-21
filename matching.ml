open Types

(* Compilation of rewrite rules to decision tree as in:
 * Compiling Pattern Matching to Good Decision Trees (Maranget, 2008)
 * *)
(*
module H = Hashtbl.Make(
struct
  type t        = ident*ident
  let hash      = Hashtbl.hash
  let equal (m,v) (m',v') =
    ident_eq v v' && ident_eq m m'
end )
 *)
(* *** Types *** *)
(*
type pMat = line * line list
type partition = { cases:( (ident*ident) * pMat ) list ; default: line list ; }
 *)
(* *** Debug *** *)
(*
let string_of_line id l = 
  let s =
    " [" ^ string_of_int l.env_size ^ "] " ^ string_of_ident id ^ " " 
    ^ (String.concat " " (Array.to_list (Array.map Pp.string_of_pattern l.pats)) )
    ^ " --> " ^ Pp.string_of_term l.right
  in
  let aux (t1,t2) = Pp.string_of_term t1 ^ " ~= " ^ Pp.string_of_term t2 in
    match l.constr with
      | []        -> s
      | _::_      -> s ^ " when " ^ (String.concat " and " (List.map aux l.constr))
 *)
(* *** Internal functions *** *)
(*
let get_constraints size_env p =
  let vars = Array.make size_env false in
  let rec aux (k,lst,pat) =
    match pat with
      | Pattern (md,id,args)    -> 
          let n = Array.length args in
          let args2 = Array.make n (Var(None,0)) in
          let aux_pat (i,kk,ll) pp = 
            let (kk',ll',p) = aux (kk,ll,pp) in
              args2.(i) <- p ;
              (i+1,kk',ll')
          in
          let (_,k',lst') = Array.fold_left aux_pat (0,k,lst) args in
            ( k', lst', Pattern (md,id,args2) )
      | Var (Some id,n)              -> 
          begin
            if vars.(n) then 
              ( k+1, (mk_DB id n,mk_DB id k)::lst, Var (Some id,k) )
            else (
              vars.(n) <- true ;
              ( k, lst, Var (Some id,n) ) )
          end
      |  j            -> ( k, lst, j ) 
  in
    aux (size_env,[],p)

let linearize k args = 
  match get_constraints k (Pattern (empty,empty,args)) with
    | k' , lst , Pattern (_,_,args')    -> ( k' , args' , lst )
    | _, _, _                           -> assert false

let line_from_rule (r:rule) : line =
  let k0 = List.length r.ctx in 
  let (k,pats,lst) = linearize k0 r.args in
    (* ( match lst with | [] -> () | _ ->(* non-linearity *) () ); *)
    { loc = r.l; pats = pats; right = r.ri; env_size = k; constr = lst }

let pMat_from_rules (rs:rule list) : int*pMat =
  match rs with
    | []        -> assert false
    | l1::tl    -> 
        let arity = Array.length l1.args in
        let aux li =
          if Array.length li.args != arity then
            Global.fail li.l "All the rules must have the same arity."
          else if (not (ident_eq l1.id li.id)) then
            Global.fail li.l "All the rules must have the same head symbol."
          else
            line_from_rule li
        in
          ( arity , ( line_from_rule l1 , List.map aux tl ) ) 

let qm = hstring "?"
let mk_var_lst inf sup =
  let rec aux i =
    if i<sup then
      (mk_DB qm i) :: (aux (i+1))
    else []
  in aux inf

let specialize (c:int) (l1,tl:pMat) (nargs,m,v:int*ident*ident) =
  let n = Array.length l1.pats  in
  let new_n = n + nargs -1      in
  (* assert ( c < n );   *)
  (* assert ( 0 <= new_n );*) 
  let mk_pats p = 
    Array.init new_n (
      fun i ->
        if i < c          then  p.(i)   (* [ 0 - (c-1) ] *)
        else if i < (n-1) then  p.(i+1) (* [ c - (n-2) ] *)
        else (* i < new_n *)    Var (None,0) (* [ (n-1) - (n+nargs-2) ] *) ) 
  in
  let spec_aux li lst =
    (*  assert ( Array.length li.pats = n ); *)
    match li.pats.(c) with
      | Var (None,_)                 ->
          { li with pats = mk_pats li.pats }::lst
      | Pattern (m',v',args)    ->
          if not (ident_eq v v' && ident_eq m m') then lst
          else (
            let pats = mk_pats li.pats in
              (* assert (Array.length args = nargs); *)
              Array.iteri (fun i a -> pats.(n-1+i) <- a) args ;
              { li with pats = pats }::lst )
      | Var (_,q)               ->
          let pats = mk_pats li.pats in
            for i=0 to (nargs-1) do
              pats.(n-1+i) <- Var (Some qm,li.env_size+i)
            done ;
            let te =
              if nargs=0 then mk_Const m v
              else mk_App ( (mk_Const m v)::
                            (mk_var_lst li.env_size (li.env_size+nargs) ) ) in
              { li with 
                    env_size  = li.env_size + nargs;
                    pats = pats; 
                    right = Subst.subst_q (q,te) 0 li.right;
                    constr = List.map (
                      fun (t1,t2) -> 
                        (Subst.subst_q (q,te) 0 t1, Subst.subst_q (q,te) 0 t2 ) 
                    ) li.constr;
              }::lst
  in
    match List.fold_right spec_aux (l1::tl) [] with
      | []              -> assert false
      | l1'::tl'        -> ( (m,v) , (l1',tl') )

let partition (l1,pm:pMat) (c:int) : partition =
  let hs = H.create 17 in
  let (def,consts) = 
    List.fold_right (
      fun l (def,csts) -> 
        (* assert ( c < Array.length l.pats ); *)
        match l.pats.(c) with
          | Pattern (m,v,args)      -> 
              if H.mem hs (m,v) then (def,csts) 
              else ( H.add hs (m,v) () ; (def,(Array.length args,m,v)::csts) )
          | _                       -> (l::def,csts)
    ) (l1::pm) ([],[]) 
  in
    { cases   = List.map (specialize c (l1,pm)) consts ; 
      default = def }

let getColumn (l:pattern array) : int option =
  let rec aux i =
    if i < Array.length l then
      match l.(i) with
        | Pattern _     -> Some i
        | _             -> aux (i+1) 
          else None
  in aux 0

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
         | Var (Some _,n) -> 
             ( (*assert ( n<l.env_size && ord.(n)=(-1)) ;*) ord.(n) <- i ) 
         | Var (None,_)   -> ()
         | Pattern _ -> assert false
      ) l.pats ;
    ord

let rec cc (pm:pMat) : gdt =
  let (l1,tl) = pm in
    match getColumn l1.pats with
      (* La 1ere ligne ne contient que des Var *)
      | None      ->
          let ord = get_order l1 in
          let right = reorder l1.loc ord l1.right in
          let constr = List.map (
            fun (t1,t2) -> ( reorder l1.loc ord t1 , reorder l1.loc ord t2 ) 
          ) l1.constr in ( 
            match constr , tl with 
              | [] , _ 
              | _ , []          -> Test (constr,right,None)
              | _ , l2::tl2     -> Test (constr,right,Some (cc (l2,tl2)))
          )
      (* Colonne c contient un pattern *)
      | Some c    ->
          let par = partition pm c in
            Switch ( c , List.rev_map (fun (id,pm') -> (id,cc pm') ) par.cases, 
                     match par.default with [] -> None | l'::pm' -> Some (cc (l',pm') ) 
            )

let rec find (m0,v0) = function
  | []                  -> None
  | ((m,v),pm)::l       ->
      if ( ident_eq v0 v && ident_eq m0 m ) then Some pm
      else find (m0,v0) l

let rec add_lines pm = function
  | Test ([],_,_) as g          -> 
      ( Global.debug 1 (fst pm).loc "Useless rule." ; g )
  | Test (lst,te,None)          -> Test (lst,te,Some (cc pm))
  | Test (lst,te,Some g)        -> Test (lst,te,Some (add_lines pm g))
  | Switch (i,cases,def)        ->
      begin
        let p = partition pm i in
        (*On met a jour les cas existant*)
        let updated_cases = List.map (
          fun (mv,g) -> 
            match find mv p.cases with
              | None            -> (mv,g)
              | Some pm'        -> (mv,add_lines pm' g)
        ) cases in
        (*On ajoute les nouveaux cas *)
        let cases2 = List.fold_left (
          fun lst ((m,v),pm') -> 
            if not (List.exists (fun ((m',v'),_) -> 
                                   ident_eq v v' && ident_eq m m') cases) then
              ((m,v),cc pm')::lst
            else lst
        ) updated_cases p.cases in
        (*On met a jour le cas par defaut*)
        let def2 = match ( def , p.default ) with
          | _      , []         -> def
          | None   , l::tl      -> Some (cc (l,tl))
          | Some g , l::tl      -> Some (add_lines (l,tl) g)
        in
          Switch (i,cases2,def2)
      end 

let add_rw (n,g) rs =
  let ( nb_args , pm ) = pMat_from_rules rs in
    if nb_args = n then add_lines pm g 
    else
      Global.fail (fst pm).loc 
        "Arity mismatch: all the rules must have the same arity." 

let get_rw id (rs:rule list) : int*gdt =
  let ( nb_args , pm )  = pMat_from_rules rs in
  
  let gdt = cc pm in
    ( nb_args , gdt )
    *)

type line = 
    { loc:loc ; pats:pattern array ; right:term ; 
      constraints:(term*term) list ; env_size:int ; }

(* Linearize + get cond *)
let get_constraints (pats:pattern array) : int * pattern array * (term*term) list = 
  assert false (*TODO*)

let line_of_rule (r:rule) : line = 
  let (size,pats,lst) = get_constraints r.args in
    { loc=r.l ; pats=pats ; right=r.rhs ; constraints=lst ; env_size=size ; }
(*
let specialize (c:int) (nargs:int) (line:line) : line = 
  let old_n = Array.length line.pats in
  let new_n = old_n + nargs -1    in
  (* assert ( c < old_n ); *)
  (* assert ( 0 <= new_n );*) 
  let new_pats =
    Array.init new_n (
      fun i ->
        if i < c              then  line.pats.(i)   (* [ 0 - (c-1) ] *)
        else if i < (old_n-1) then  line.pats.(i+1) (* [ c - (n-2) ] *)
        else (* i < new_n *)    EVar (* [ (n-1) - (n+nargs-2) ] *) ) 
  in
    match line.pats.(c) with
      | EVar                    -> { line with pats = new_pats }   (*FIXME delete ?*)
      | Pattern (m',v',args)    ->
          begin
            Array.iteri (fun i a -> new_pats.(old_n-1+i) <- a) args ;
            { line with pats = new_pats }
          end
      | Var (_,q)               ->
          assert false (*TODO*)
            (* On a remplace par des var fresh et on update les contraintes
          let pats = mk_pats li.pats in
            for i=0 to (nargs-1) do
              pats.(n-1+i) <- Var (Some qm,li.env_size+i)
            done ;
            let te =
              if nargs=0 then mk_Const m v
              else mk_App ( (mk_Const m v)::
                            (mk_var_lst li.env_size (li.env_size+nargs) ) ) in
              { li with 
                    env_size  = li.env_size + nargs;
                    pats = pats; 
                    right = Subst.subst_q (q,te) 0 li.right;
                    constr = List.map (
                      fun (t1,t2) -> 
                        (Subst.subst_q (q,te) 0 t1, Subst.subst_q (q,te) 0 t2 ) 
                    ) li.constr;
              }::lst
             *)
      | Condition (_,_)         -> assert false
 *)

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

let reorder l (ord:int array) (t:term) : term = assert false (* TODO
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
  in aux 0 t *)

let get_order (l:line) : int array = assert false (* TODO
  let ord = Array.make l.env_size (-1) in
    Array.iteri 
      (fun i p -> match p with
         | Var (Some _,n) -> 
             ( (*assert ( n<l.env_size && ord.(n)=(-1)) ;*) ord.(n) <- i ) 
         | Var (None,_)   -> ()
         | Pattern _ -> assert false
      ) l.pats ;
    ord *)

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
