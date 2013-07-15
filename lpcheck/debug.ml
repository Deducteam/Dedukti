
open Types

let string_of_loc (l,c) = "[l:"^string_of_int l^";c:"^string_of_int c^"]"

let rec string_of_term : term -> string = function
  | Kind        -> "Kind"
  | Type        -> "Type"
  | DB  n       -> string_of_int n
  | GVar (m,v)  -> m^"."^v
  | RVar v      -> "(RW "^v^")"
  | LVar n      -> "var"^string_of_int n
  | App args    -> "(" ^ String.concat " " (List.map string_of_term args) ^ ")"
  | Lam (a,f)   -> "(\ "^string_of_term a^" => "^string_of_term f^")"
  | Pi  (a,b)   -> "(" ^ string_of_term a^" -> "^string_of_term b ^")" 
