open Types

let cpt_rule = ref 0
let get_rule_nb _ = incr cpt_rule ; !cpt_rule
let clear_rule_counter _ = cpt_rule := 0

let rec is_type_level = function
  | Pi (_,_,t)  -> is_type_level t
  | Type        -> true
  | _           -> false

let rec check_meta = function
  | Kind | Type | DB _ | Const _ -> true
  | Meta _                      -> false
  | Pi (_,a,b) | Lam (_,a,b)    -> check_meta a && check_meta b
  | App lst                     -> List.for_all check_meta lst

let exists f arr =
  let rec aux i =
    if i< (Array.length arr) then (
      if f (arr.(i)) then true
      else aux (i+1)
    ) else false
  in aux 0

let rec has_definitions = function
  | Var _               -> false
  | Pattern (m,v,args)  ->
      ( match Env.get_global_symbol dloc m v with
          | Env.Def (_,_)   -> true
          | _           -> exists has_definitions args )
(*
let dump =
  List.iter (fun (t,t') -> (Global.debug 2) dloc " %s == %s\n" 
                             (Pp.string_of_term t) (Pp.string_of_term t') )
 *)
let check_rule (pctx,ple,pri:prule) : rule =
  let (l,id,_) = ple in
  let (ctx,k0) =
    List.fold_left
      (fun (ctx,i) (_,x,ty) -> ((x,Inference.check_type ctx ty)::ctx,i+1))
      ([],0) pctx in
  let (k,le,ty0,eqs) = Inference.infer_ptop ctx ple in
  let args = match le with
    | Var _                   -> 
        Global.fail l "The left-hand side of the rewrite rule cannot be a variable."
    | Pattern (_,_,args)      -> args
  in
    match Unification.unify eqs with
      | Unification.NoUnifier           ->  
          Global.fail l "The pattern '%a' is not well-typed." 
            Pp.pp_pattern (Pattern (!Global.name,id,args))
      | Unification.UPrefix s            (*FIXME warning*)
      | Unification.MGU s               ->
          let ty = Subst.subst_meta s ty0 in
            if not (Inference.is_well_typed ctx ty) then
              Global.fail l "Could not find a closed type for '%a'\nInferred type: %a." 
                Pp.pp_pattern (Pattern (!Global.name,id,args)) Pp.pp_term ty
            else if has_definitions le then
                Global.fail l "Defined symbols are not allowed in patterns."
            else
              begin
                let ri = Inference.check_term ctx pri ty  in
                  { nb=get_rule_nb (); l=l; ctx=ctx; id=id; args=args;
                    ri=ri; sub=s; k=k; md= !Global.name; }
              end
