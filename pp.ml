
open Types

let string_of_const m v =
  if ident_eq m !Global.name then string_of_ident v
  else string_of_ident m ^ "." ^ string_of_ident v

let string_of_const2 md_opt id =
  match md_opt with
    | None          -> string_of_ident id
    | Some md       -> string_of_ident md ^ "." ^ string_of_ident id

let rec string_of_pterm = function
  | PreType _              -> "Type"
  | PreId (_,v)            -> string_of_ident v
  | PreQId (_,m,v)         -> string_of_ident m ^ "." ^ string_of_ident v
  | PreApp args            ->
      String.concat " " (List.map string_of_pterm_wp args)
  | PreLam (_,v,a,b)       ->
     string_of_ident v^ ":" ^ string_of_pterm_wp a ^ " => " ^ string_of_pterm b
  | PrePi (None,a,b)       -> string_of_pterm_wp a ^ " -> " ^ string_of_pterm b
  | PrePi (Some (_,v),a,b) ->
     string_of_ident v^ ":" ^ string_of_pterm_wp a ^ " -> " ^ string_of_pterm b
and string_of_pterm_wp = function
 | PreType _ | PreId _ | PreQId _ as t  -> string_of_pterm t
 | t                                    -> "(" ^ string_of_pterm t ^ ")"

let rec string_of_term = function
  | Kind                -> "Kind"
  | Type                -> "Type"
  | Meta n              -> "?[" ^ string_of_int n ^ "]"
  | DB  (x,n)           -> string_of_ident x^"["^string_of_int n^"]"
  | Const (m,v)         -> string_of_const m v
  | App args            -> String.concat " " (List.map string_of_term_wp args)
  | Lam (x,a,f)         ->
      string_of_ident x ^ ":" ^ string_of_term_wp a ^ " => " ^ string_of_term f
  | Pi  (None,a,b)      -> string_of_term_wp a ^ " -> " ^ string_of_term b
  | Pi  (Some x,a,b)    ->
     string_of_ident x ^ ":" ^ string_of_term_wp a ^ " -> " ^ string_of_term b
and string_of_term_wp = function
  | Kind | Type _  | DB _ | Const _ as t        -> string_of_term t
  | t                                           -> "(" ^ string_of_term t ^ ")"

let rec string_of_prepattern = function
  | Unknown _                   -> "_"
  | PPattern (_,md_opt,id,[])   -> string_of_const2 md_opt id
  | PPattern (_,md_opt,id,lst)  ->
      "(" ^ string_of_const2 md_opt id ^ " " ^
      (String.concat " " (List.map string_of_prepattern lst)) ^ ")"

let rec string_of_pattern_sub s = function
  | Var (Some id,v)          -> string_of_ident id ^ "[" ^ string_of_int v ^ "]"
  | Var (None, n)             ->
      ( try "{" ^ string_of_term (List.assoc n s) ^ "}"
        with Not_found -> "_[" ^ string_of_int n ^ "]" )
  | Pattern (m,v,arr)   ->
      if Array.length arr = 0 then string_of_const m v
      else
        string_of_const m v ^ " " ^
        String.concat " " (List.map (string_of_pattern_wp s) (Array.to_list arr))

and string_of_pattern_wp s = function
  | Pattern (_,_,args) as p when (Array.length args != 0) ->
      "(" ^ string_of_pattern_sub s p ^ ")"
  |  p -> string_of_pattern_sub s p

let string_of_pattern = string_of_pattern_sub []

let string_of_rule r =
  "["^string_of_int r.nb ^"] "
  ^ string_of_pattern_sub r.sub (Pattern (!Global.name,r.id,r.args))
  ^ " --> " ^ string_of_term r.ri

let string_of_cpair cp =
  let pos = String.concat "." (List.map string_of_int cp.pos) in
  "Rule (" ^ string_of_int cp.rule1 ^ ") and (" ^ string_of_int cp.rule2 ^
  ") overlap at pos:" ^ pos ^" \n"
  ^ string_of_pattern cp.root ^ " --> " ^ string_of_term cp.red1 ^ "\n"
  ^ string_of_pattern cp.root ^ " --> " ^ string_of_term cp.red2 ^ "\n"
  ^ "Joinability: " ^ (if cp.joinable then "OK" else "KO")

let tab t = String.make (t*4) ' '

let rec str_of_gdt t = function
  | Test ([],te,_)              -> string_of_term te
  | Test (lst,te,def)           ->
      begin
        let str = "\n" ^ tab t ^ "if "
        ^ String.concat " and "
            (List.map (fun (i,j) -> string_of_term i ^ "=" ^ string_of_term j ) lst)
        ^ " then " ^ string_of_term te
        in
          match def with
            | None      -> str ^ "\n" ^ tab t ^ "else FAIL"
            | Some g    -> str ^ "\n" ^ tab t ^ "else " ^ str_of_gdt (t+1) g
      end
    | Switch (i,cases,def)      ->
        begin
          let str_lst =
            List.map
              (fun ((m,v),g) ->
                 "\n" ^ tab t ^ "if $" ^ string_of_int i ^ "="
                 ^ string_of_const m v ^ " then " ^ str_of_gdt (t+1) g
              ) cases in
          let str = String.concat "" str_lst in
            match def with
              | None    -> str ^ "\n" ^ tab t ^ "default: FAIL"
              | Some g  -> str ^ "\n" ^ tab t ^ "default: " ^ str_of_gdt (t+1) g
        end

let string_of_gdt m v i g =
  "GDT for '" ^ string_of_const m v ^ "' with " ^ string_of_int i
  ^ " argument(s): " ^ str_of_gdt 0 g
