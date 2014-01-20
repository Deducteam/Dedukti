open Types

let rec pattern_of_term = function
  | DB (id,n)                   -> Var (id,n)
(*  | Meta n                      -> Joker n *)
  | Const (md,id)               -> Pattern (md,id,[||])
  | App ((Const (md,id))::args) -> Pattern (md,id,Array.of_list (List.map pattern_of_term args))
  | App ((DB _)::args)          -> assert false
  | _                           -> assert false

let get_constraints size_env p =
  let vars = Array.make size_env false in
  let rec aux (k,lst,pat) =
      match pat with
        | Pattern (md,id,args)        -> 
            let args2 = Array.make (Array.length args) (Joker 0) in
            let aux_pat (i,kk,ll) pp = 
              let (kk',ll',p) = aux (kk,ll,pp) in
                args2.(i) <- p ;
                (i+1,kk',ll')
            in
            let (_,k',lst') = Array.fold_left aux_pat (0,k,lst) args in
              ( k', lst', Pattern (md,id,args2) )
        | Var (id,n)                  -> 
            begin
              if vars.(n) then 
                ( k+1, (mk_DB id n,mk_DB id k)::lst, Var (id,k) )
              else (
                vars.(n) <- true ;
                ( k, lst, Var (id,n) ) )
            end
        | Joker n                     -> ( k, lst, Joker n ) 
  in
    aux (size_env,[],p)

let get_top_pattern_with_constraints l size_env t =
  let p = pattern_of_term t in
    match get_constraints size_env p with
      | ( _ , _ , Joker _ )                     -> assert false (*FIXME cannot happen ?*) 
      | ( _ , _ , Var _ )                       -> raise (PatternError ( l , "The left-hand side of a rule cannot be a variable." ))
      | ( k , lst , Pattern (_,id,args) )       ->  (k,id,args,lst)


type ustate = (term*term) list (* Terms to unify *)
            * (int*term)  list (* Variable to substitute *)
            * (int*term)  list (* Substitution *)

let rec not_in n : term -> bool = function
  (*| Meta i                              -> i <> n*)
  | Kind | Type _ | Const _ | DB _      -> true
  | App args                            -> List.for_all (not_in n) args
  | Lam (_,ty,te) | Pi (_,ty,te)        -> not_in n ty && not_in n te

let rec unify (lc:loc) : ustate -> (int*term) list = assert false (*function
  | ( [] , [] , s)              -> s
  | ( [] , (v,t0)::b , s)       ->
      let t = Subst.meta_subst 0 s t0 in
        if not_in v t then
          begin
            try unify lc ( [(t,List.assoc v s)] , b , s )
            with Not_found ->
              unify lc ( [] , b , (v,t)::(List.map (fun (z,te) -> (z,Subst.meta_subst 0 [(v,t)] te)) s) )
        end
      else
        raise (PatternError ( lc , "Cannot find a type." ))
  | ( (t1,t2)::a , b , s )      ->
      begin
        match Reduction.decompose_eq t1 t2 with
         | None         -> raise (PatternError ( lc , "Cannot find a type." ))
         | Some lst     -> unify lc (a,lst@b,s)
      end *)

let rec check_term = function
  | Kind | Type _ | Const _ | DB _      -> true
    (*  | Meta _                              -> false*)
  | App args                            -> List.for_all check_term args
  | Lam (_,ty,te) | Pi (_,ty,te)        -> check_term ty && check_term te

(*
let rec print_subst = function
  | []          -> ()
  | (i,t)::lst  ->
      Global.eprint ( string_of_int i ^ " => " ^ Pp.string_of_term2 t ) ;
      print_subst lst
*)

let resolve_type lc ty lst = assert false (*
  let s = unify lc (lst,[],[]) in
    (* print_subst s ; *)
  let sty = Subst.meta_subst 0 s ty in
    if check_term sty then sty
    else raise ( PatternError ( lc , "Cannot find a type." ) ) *)
