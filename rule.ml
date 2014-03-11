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
(*
let rec has_definitions = function
  | Var _               -> false
  | Pattern (m,v,args)  ->
      ( match Env.get_global_symbol dloc m v with
          | Env.Def (_,_)   -> true
          | _           -> exists has_definitions args )
 *)
 (*
let dump =
  List.iter (fun (t,t') -> (Global.debug 2) dloc " %s == %s\n" 
                             (Pp.string_of_term t) (Pp.string_of_term t') )
 *)

