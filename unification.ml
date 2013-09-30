
open Types

type ustate = (term*term) list * (int*term) list * (int*term) list

let rec check (k:int) (v:int) : term -> unit = function
  | Kind | Type | GVar _        -> ()
  | DB i                        -> assert (i != v+k ) (*FIXME*)
  | App args                    -> List.iter (check k v) args 
  | Lam (ty,te) | Pi (ty,te)    -> ( check k v ty ; check (k+1) v te )
 
let rec unify (rw:int) : ustate -> (int*term) list = function
  | ( [] , [] , s)              -> s
  | ( [] , (v,t)::b , s)        -> 
      begin
        check 0 v t ; (*FIXME *)
        (try  unify rw ([(t,List.assoc v s)],b,s)
         with Not_found ->
           unify rw ([],b,(v,t)::(List.map (fun (z,te) -> (z,Subst.subst2 [(v,t)] te)) s)) )
      end
  | ( (t1,t2)::a , b , s )      -> unify rw (a,Reduction.decompose rw b t1 t2,s)

let get_unifier (k:int) (ctx:term list) (p:pattern) : term*(int*term) list =
  (* assert (List.length ctx == k ); *)
  let ( ty , cstr_lst ) = Inference.infer_pattern ctx p in
  let subst             = unify k (cstr_lst,[],[]) in
    ( Subst.subst2 subst ty , subst )
