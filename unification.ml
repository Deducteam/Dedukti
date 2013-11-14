(* TODO delete

open Types

exception CannotFindAType
exception CannotType 

type unif_result =
  | Success of term
  | Failure
  | DontKnow


type ustate = (term*term) list (* Terms to unify *)
            * (int*term)  list (* Variable to substitute *)
            * (int*term)  list (* Substitution *)

let rec not_in n : term -> bool = function
  | Meta (_,i)                          -> ( i <> n )
  | Kind | Type _ | GVar _ | DB _       -> true
  | App args                            -> List.for_all (not_in n) args 
  | Lam (_,_,ty,te) | Pi (_,_,ty,te)    -> not_in n ty && not_in n te

let rec subst k (lst:(int*term) list) (te:term) : term = (*TODO mettre dans subst.ml *)
    match te with
      | Kind | Type _ | GVar _ | DB _   -> te
      | Meta (_,n)                      -> 
          ( try Subst.shift k 0 (List.assoc n lst)
            with Not_found -> te )
      | App args                        -> mk_app ( List.map (subst k lst) args )
      | Lam (l,x,a,b)                   -> mk_lam l x ( subst k lst a ) ( subst (k+1) lst b )
      | Pi  (l,x,a,b)                   -> mk_pi  l x ( subst k lst a ) ( subst (k+1) lst b )

let rec unify : ustate -> (int*term) list = function
  | ( [] , [] , s)              -> s
  | ( [] , (v,t0)::b , s)       -> 
      let t = subst 0 s t0 in
        if not_in v t then
          begin
            try unify ( [(t,List.assoc v s)] , b , s )
            with Not_found ->
              unify ( [] , b , (v,t)::(List.map (fun (z,te) -> (z,subst 0 [(v,t)] te)) s) ) 
        end
      else
        raise CannotType
  | ( (t1,t2)::a , b , s )      -> 
      begin
        match Reduction.decompose_eq t1 t2 with
         | None         -> raise CannotType
         | Some lst     -> unify (a,lst@b,s)
      end

let rec check_term = function
  | Kind | Type _ | GVar _ | DB _       -> true
  | Meta _                              -> false
  | App args                            -> List.for_all check_term args 
  | Lam (_,_,ty,te) | Pi (_,_,ty,te)    -> check_term ty && check_term te

let rec print_subst = function
  | []          -> ()
  | (i,t)::lst  ->
      Global.eprint ( string_of_int i ^ " => " ^ Pp.string_of_term2 t ) ;
      print_subst lst

let resolve_constraints ty lst = assert false 
  let s = unify (lst,[],[]) in
    (* print_subst s ; *)
  let sty = subst 0 s ty in
    if check_term sty then sty
    else raise CannotFindAType 
    *)



