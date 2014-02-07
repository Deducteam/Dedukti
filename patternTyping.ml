
open Types

let is_empty = function []  -> true | _   -> false

let infer (ctx:context) (l,id,args:ptop) : top*term = 
  match get_type ctx id with
    | Some (n,ty) -> 
        if is_empty args then 
          raise (PatternError (l,"The left-hand side of the rewrite rule cannot be a variable."))
        else 
          raise (PatternError (l,"The left-hand side of the rewrite rule cannot be a variable application."))
    | None        -> 
        let ty_id = Env.get_global_type l !Global.name id in
        let (ty0,args0,eqs) = 
          List.fold_left (check_pattern_args l None id ctx) (ty_id,[],[]) args in
        let s = Unification.unify_t eqs in
        let ty  = check_meta (subst s ty0) in (*FIXME should be typed-checked*)
        let args = List.rev_map (subst s) args0 in
          ( ( id , Array.of_list args ) , ty ) 


