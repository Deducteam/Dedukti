
open Types

(* TODO rename in pp *)  

  (*
let rec string_of_pterm = function
  | PType _                     -> "Type"
  | PId (_,v)                   -> string_of_ident v
  | PQid (_,m,v)                -> string_of_ident m ^ "." ^ string_of_ident v
  | PApp ([f;u])                  -> "(" ^ string_of_pterm f ^ " " ^ string_of_pterm u ^ ")"
  | PApp (_)                    -> 
  | PLam ((_,v),None,te)        -> "(Lam " ^ string_of_ident v ^ " => " ^ string_of_pterm te ^ ")"
  | PLam ((_,v),Some ty,te)     -> "(Lam " ^ string_of_ident v ^ ":" ^ string_of_pterm ty ^ " => " ^ string_of_pterm te ^ ")"
  | PPi (None,ty,te)            -> "(Pi _:" ^ string_of_pterm ty ^ " -> " ^ string_of_pterm te ^ ")"
  | PPi (Some (_,v),ty,te)      -> "(Pi " ^ string_of_ident v ^ ":" ^ string_of_pterm ty ^ " -> " ^ string_of_pterm te ^ ")"
   *)

let rec string_of_term2 = function 
  | Kind                        -> "Kind"
  | Type _                      -> "Type"
  | DB  (_,x,n)                 -> string_of_ident x^"["^string_of_int n^"]"
  | GVar (_,m,v) 
      when (ident_eq m !Global.name) -> string_of_ident v 
  | GVar (_,m,v)                -> string_of_ident m ^ "." ^ string_of_ident v
  | App args                    -> String.concat " " (List.map string_of_term2_wp args)
  | Lam (_,x,a,f)               -> string_of_ident x ^ ":" ^ string_of_term2_wp a ^ " => " ^ string_of_term2 f
  | Pi  (_,x,a,b) 
      when ident_eq x empty     -> string_of_term2_wp a ^ " -> " ^ string_of_term2 b
  | Pi  (_,x,a,b)               -> string_of_ident x ^ ":" ^   string_of_term2_wp a ^ " -> " ^ string_of_term2 b
  | Meta (_,_)                  -> "_"
and string_of_term2_wp = function 
  | Kind | Type _  | DB _ | GVar _ | Meta _ as t -> string_of_term2 t
  | t   -> "(" ^ string_of_term2 t ^ ")"

let rec string_of_term = function 
  | Kind                        -> "Kind"
  | Type _                      -> "Type"
  | DB  (_,x,_)                 -> string_of_ident x
  | GVar (_,m,v) 
      when (ident_eq m !Global.name) -> string_of_ident v 
  | GVar (_,m,v)                -> string_of_ident m ^ "." ^ string_of_ident v
  | App args                    -> String.concat " " (List.map string_of_term_wp args)
  | Lam (_,x,a,f)               -> string_of_ident x ^ ":" ^ string_of_term_wp a ^ " => " ^ string_of_term f
  | Pi  (_,x,a,b) 
      when ident_eq x empty     -> string_of_term_wp a ^ " -> " ^ string_of_term b
  | Pi  (_,x,a,b)               -> string_of_ident x ^ ":" ^   string_of_term_wp a ^ " -> " ^ string_of_term b
  | Meta (_,_)                  -> "_"
and string_of_term_wp = function 
  | Kind | Type _  | DB _ | GVar _ | Meta _ as t -> string_of_term t
  | t   -> "(" ^ string_of_term t ^ ")"

let rec string_of_pattern = function 
  | Var (_,_,v)                 -> string_of_int v
  | Dash _                      -> "_"
  | Pattern ((_,m,v),arr)       -> 
      let cst = if ident_eq m !Global.name then string_of_ident v else ( string_of_ident m ^ "." ^ string_of_ident v ) in
        cst ^ " " ^ String.concat " " (List.map string_of_pattern_wp (Array.to_list arr))
and string_of_pattern_wp = function
  | Var _ | Dash _ as p -> string_of_pattern p
  | p                   -> "(" ^ string_of_pattern p ^ ")"
   
