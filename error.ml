
open Types

(* *** Strings *** *)

let rec string_of_pterm = function
  | PType _                     -> "Type"
  | PId (_,v)                   -> string_of_ident v
  | PQid (_,m,v)                -> string_of_ident m ^ "." ^ string_of_ident v
  | PApp (f,u)                  -> "(" ^ string_of_pterm f ^ " " ^ string_of_pterm u ^ ")"
  | PLam ((_,v),None,te)        -> "(Lam " ^ string_of_ident v ^ " => " ^ string_of_pterm te ^ ")"
  | PLam ((_,v),Some ty,te)     -> "(Lam " ^ string_of_ident v ^ ":" ^ string_of_pterm ty ^ " => " ^ string_of_pterm te ^ ")"
  | PPi (None,ty,te)            -> "(Pi _:" ^ string_of_pterm ty ^ " -> " ^ string_of_pterm te ^ ")"
  | PPi (Some (_,v),ty,te)      -> "(Pi " ^ string_of_ident v ^ ":" ^ string_of_pterm ty ^ " -> " ^ string_of_pterm te ^ ")"

let rec string_of_term = function
  | Kind        -> "Kind"
  | Type        -> "Type"
  | DB  n       -> "DB" ^ string_of_int n
  | GVar (m,v)  -> string_of_ident m ^ "." ^ string_of_ident v
  | App args    -> "(" ^ String.concat " " (List.map string_of_term args) ^ ")"
  | Lam (a,f)   -> "(\ " ^ string_of_term a ^ " => " ^ string_of_term f ^ ")"
  | Pi  (a,b)   -> "(" ^   string_of_term a ^ " -> " ^ string_of_term b ^ ")" 
  | Meta _      -> "_"

let rec string_of_pattern = function
  | Var v                       -> string_of_int v
  | Dash _                      -> "_"
  | Pattern ((_,m,v),arr)       -> "(" ^ string_of_ident m ^ "." ^ string_of_ident v ^ " " ^ String.concat " " (List.map string_of_pattern (Array.to_list arr)) ^ ")"

(* *** Typing Errors *** *)

let err_conv_type te inf =  
  "Error while typing "^string_of_term te ^".\nExpected type: Type.\nInferred type: "^string_of_term inf^".\n"

let err_conv te exp inf =  
  "Error while typing "^string_of_term te ^".\nExpected type: "^string_of_term exp^".\nInferred type: "^string_of_term inf^".\n"

let err_conv2 te exp inf exp' inf' =  
  "Error while typing "^string_of_term te ^".\nExpected type: "^string_of_term exp^" [ "^string_of_term exp'^" ].\nInferred type: "^string_of_term inf^" [ "^string_of_term inf'^" ].\n"

let err_sort te ty =
  "Error while typing "^string_of_term te ^".\nExpected type: Type or Kind.\nInferred type: "^string_of_term ty^".\n"

let err_topsort te = 
  "Error while typing "^string_of_term te ^".\nExpected type: anything but Kind.\nInferred type: Kind.\n"

let err_prod te ty = 
  "Error while typing "^string_of_term te ^".\nProduct expected.\nInferred type: "^string_of_term ty^".\n"

let err_prod2 ty = 
  "Product expected.\nInferred type: "^string_of_term ty^".\n"

let err_rule (c,a) = 
  "Error while typing "^string_of_pattern (Pattern (c,a)) ^".\nCannot find a type.\n"

(* *** Rule Errors *** *)
