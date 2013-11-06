open Types

exception UExcn 

type ustate = (term*term) list (* Terms to unify *)
            * (int*term)  list (* Variable to substitute *)
            * (int*term)  list (* Substitution *)

let rec not_in (k:int) (v:int) : term -> bool = function
  | Kind | Type _ | GVar _ | DB _       -> true
  | Meta (_,i)                          -> (i != v+k )
  | App args                            -> List.for_all (not_in k v) args 
  | Lam (_,_,ty,te) | Pi (_,_,ty,te)    -> not_in k v ty && not_in (k+1) v te

let rec subst k (lst:(int*term) list) (te:term) : term = (*TODO mettre dans subst.ml *)
    match te with
      | Kind | Type _ | GVar _ | DB _   -> te
      | Meta (_,n)                      -> 
          ( try Subst.shift k 0 (List.assoc n lst)
            with Not_found -> raise UExcn )
      | App args                        -> mk_app ( List.map (subst k lst) args )
      | Lam (l,x,a,b)                   -> mk_lam l x ( subst k lst a ) ( subst (k+1) lst b )
      | Pi  (l,x,a,b)                   -> mk_pi  l x ( subst k lst a ) ( subst (k+1) lst b )

let rec unify : ustate -> (int*term) list = function
  | ( [] , [] , s)              -> s
  | ( [] , (v,t)::b , s)        -> 
      if not_in 0 v t then
        begin
          try  
            unify ( [(t,List.assoc v s)] , b , s )
          with Not_found ->
            unify ( [] , b , (v,t)::(List.map (fun (z,te) -> (z,subst 0 [(v,t)] te)) s) ) 
        end
      else
        raise UExcn
  | ( (t1,t2)::a , b , s )      -> 
      begin
        match Reduction.decompose_eq t1 t2 with
         | None         -> raise UExcn
         | Some lst     -> unify (a,lst@b,s)
      end

let resolve_constraints (ty:term) (lst:(term*term) list) : term option =
  try 
    let s = unify (lst,[],[]) in
      Some (subst 0 s ty)
  with
    | UExcn     -> None



