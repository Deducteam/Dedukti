
open Types

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

let string_of_gvar m v =
  if ident_eq m !Global.name then string_of_ident v 
  else string_of_ident m ^ "." ^ string_of_ident v

let rec string_of_term2 = function 
  | Kind                        -> "Kind"
  | Type _                      -> "Type"
  | DB  (_,x,n)                 -> string_of_ident x^"["^string_of_int n^"]"
  | GVar (_,m,v)                -> string_of_gvar m v 
  | App args                    -> String.concat " " (List.map string_of_term2_wp args)
  | Lam (_,x,a,f)               -> string_of_ident x ^ ":" ^ string_of_term2_wp a ^ " => " ^ string_of_term2 f
  | Pi  (_,x,a,b) 
      when ident_eq x empty     -> string_of_term2_wp a ^ " -> " ^ string_of_term2 b
  | Pi  (_,x,a,b)               -> string_of_ident x ^ ":" ^   string_of_term2_wp a ^ " -> " ^ string_of_term2 b
  | Meta (_,n)                  -> "_["^string_of_int n^"]"
and string_of_term2_wp = function 
  | Kind | Type _  | DB _ | GVar _ | Meta _ as t -> string_of_term2 t
  | t   -> "(" ^ string_of_term2 t ^ ")"

let rec string_of_term = function 
  | Kind                        -> "Kind"
  | Type _                      -> "Type"
  | DB  (_,x,_)                 -> string_of_ident x
  | GVar (_,m,v)                -> string_of_gvar m v 
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
  | Var (_,id,v)                 -> string_of_ident id ^ "[" ^ string_of_int v ^ "]"
  | Dash _                      -> "_"
  | Pattern ((_,m,v),arr)       -> string_of_gvar m v ^ " " ^ String.concat " " (List.map string_of_pattern_wp (Array.to_list arr))
and string_of_pattern_wp = function
  | Var _ | Dash _ as p -> string_of_pattern p
  | p                   -> "(" ^ string_of_pattern p ^ ")"
 
let tab t = String.make (t*4) ' ' 

let rec str_of_gdt (t,b) = function
  | Test ([],te,_)              -> tab t ^ (if b then "else " else "") ^ string_of_term2 te 
  | Test (lst,te,def)           -> 
      begin
        let str =
          tab t ^ "if " ^ String.concat " and " (List.map (fun (i,j) -> "$"^string_of_int i^"=$"^string_of_int j^"" ) lst)
          ^ " then " ^ string_of_term2 te 
        in
          match def with
            | None      -> str
            | Some g    -> str ^ "\n" ^ str_of_gdt (t,true) g
      end
    | Switch (i,cases,def)      ->
        begin
          let str_lst = List.map (fun ((m,v),g) -> "if $"^string_of_int i^"="^string_of_gvar m v^" then\n" ^ str_of_gdt (t+1,false) g ) cases in
          let str = tab t ^ String.concat "\n" str_lst in
            match def with
              | None    -> str
              | Some g  -> str ^ "\n" ^str_of_gdt (t,true) g
        end

let string_of_gdt m v i g = 
  "GDT for " ^ string_of_gvar m v ^ " with " ^ string_of_int i ^ " argument(s):\n"
  ^ str_of_gdt (0,false) g

