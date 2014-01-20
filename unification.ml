
open Types

type substitution = (int*partial_term)  list
type ustate = (partial_term*partial_term) list (* Terms to unify *)
            * (int*partial_term)  list (* Variable to substitute *)
            * substitution 

let rec add_to_list lst s s' =
  match s,s' with
    | [] , []           -> Some lst
    | x::s1 , y::s2     -> add_to_list ((x,y)::lst) s1 s2
    | _ ,_              -> None

let constant_app_in_wnf = function
  | Term (Const (m,v))  ->
      ( match Env.get_global_symbol dloc m v with
          | Env.Decl (_,None)   -> Some (m,v)
          | _                   -> None )
  | _                   -> None

let rec decompose b = function
  | []          -> b
  | (t1,t2)::a  ->
      begin
        match t1 , t2 with
          | Meta n , t | t , Meta n                             -> 
              decompose ((n,t)::b) a
          | PartialPi  (_,ty,te), PartialPi  (_,ty',te')
          | PartialLam (_,ty,te), PartialLam (_,ty',te')        -> 
              decompose b ( (ty,ty')::(te,te')::a )
          | PartialLam (_,ty,te) , PartialPi  (_,ty',te')
          | PartialPi  (_,ty,te) , PartialLam (_,ty',te')       -> 
              assert false (*TODO error*)

          | PartialApp (f::tl) , t | t, PartialApp (f::tl)      ->
              begin
                match constant_app_in_wnf f , t with
                  | None , _                            -> 
                      decompose b a (* ici on perd de l'information *)
                  | Some (m,v) , Term te                ->
                      ( match Reduction.wnf te with
                          | App ((Const (m',v'))::tl0)  ->
                              if ident_eq v v' && ident_eq m m' then
                                let tl' = List.map mk_partial tl0 in
                                ( match add_to_list a tl tl' with
                                    | None        -> assert false (*TODO error*)
                                    | Some a'     -> decompose b a' 
                                ) else
                                assert false (*TODO error*)
                          | _                           -> 
                              assert false (*TODO error*)
                      )
                  | Some (m,v) , PartialApp (f'::tl')   ->
                      ( match constant_app_in_wnf f' with
                          | None                -> 
                              decompose b a (* ici on perd de l'information *)
                          | Some (m',v')        ->
                              if ident_eq v v' && ident_eq m m' then
                                ( match add_to_list a tl tl' with
                                    | None        -> assert false (*TODO error*)
                                    | Some a'     -> decompose b a' 
                                ) else
                                assert false (*TODO error*)
                      )
                  | _ , _                               -> assert false (*error TODO*)
              end
          | PartialApp [] , t | t, PartialApp []                -> assert false
              
          | Term tt, pt | pt, Term tt                           -> 
              begin (* pt = PartialPi | PartialLam *)
                  match pt , Reduction.wnf tt with
                    | PartialPi (_,ty,te) , Pi (_,ty',te')       
                    | PartialLam (_,ty,te) , Lam (_,ty',te')    -> 
                        decompose b ((ty,mk_partial ty')::(te,mk_partial te')::a)
                    | _ , _                                     -> 
                        assert false (*TODO error*)
              end
      end

let rec safe_assoc v = function
  | []                  -> None
  | (x,t)::_ when x=v   -> Some t
  | _::tl               -> safe_assoc v tl

let rec not_in n = function
  | Meta i              -> i <> n
  | Term _              -> true
  | PartialApp args     -> List.for_all (not_in n) args
  | PartialLam (_,ty,te) 
  | PartialPi (_,ty,te) -> not_in n ty && not_in n te 

let rec unify (lc:loc) : ustate -> substitution = function
  | ( [] , [] , s )             -> s
  | ( [] , (v,t0)::b , s)       ->
      let t = Subst.meta_subst 0 s t0 in
        if not_in v t then ( 
          match safe_assoc v s with
            | Some t'   -> unify lc ( [(t,t')] , b , s )
            | None      -> 
                let s' = List.map ( fun (z,te) -> ( z , Subst.meta_subst 0 [(v,t)] te ) ) s in
                  unify lc ( [] , b , (v,t)::s' ) ) 
        else
          raise (PatternError ( lc , "Cannot find a type." )) (*TODO error*)
  | ( a , b , s )      -> unify lc ( [] , decompose b a , s )

let resolve ty args eqs : term*pattern list =
  let s = unify dloc (eqs,[],[]) in
  let args' = List.map (Subst.meta_subst_pattern s) args in 
    match Subst.meta_subst 0 s ty with
      | Term ty'        -> ( ty' , args' )
      | _               -> assert false (*TODO error*)
