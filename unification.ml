
open Types

type ustate = (term*term) list (* Terms to unify *)
            * (int*term)  list (* Variable to substitute *)
            * (int*term)  list (* Substitution *)

let rec not_in (k:int) (v:int) : term -> bool = function
  | Kind | Type | GVar _        -> true
  | DB i                        -> (i != v+k )
  | App args                    -> List.for_all (not_in k v) args 
  | Lam (ty,te) | Pi (ty,te)    -> not_in k v ty && not_in (k+1) v te
 
let rec unify (rw:int) : ustate -> (int*term) list = function
  | ( [] , [] , s)              -> s
  | ( [] , (v,t)::b , s)        -> 
      if not_in 0 v t then
        begin
          try  
            unify rw ( [(t,List.assoc v s)] , b , s )
          with Not_found ->
            unify rw ( [] , b , (v,t)::(List.map (fun (z,te) -> (z,Subst.subst2 [(v,t)] te)) s) ) 
        end
      else
        failwith "Cannot unify." (*FIXME*)
  | ( (t1,t2)::a , b , s )      -> 
      begin
        match Reduction.state_unif b [ ( (0,[],t1,[]) , (0,[],t2,[]) ) ] with
         | None         -> failwith "Cannot unify." (*FIXME*)
         | Some lst     -> unify rw (a,lst,s)
      end

let get_unifier (k:int) (ctx:term list) (p:pattern) : term*(int*term) list =
  (* assert (List.length ctx == k ); *)
  let ( ty , cstr_lst ) = Inference.infer_pattern ctx p in
  let subst             = unify k (cstr_lst,[],[]) in
    ( Subst.subst2 subst ty , subst )
