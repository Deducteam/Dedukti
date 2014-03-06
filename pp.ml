open Types
open Printf

let rec pp_list sep pp out = function
    | []        -> ()
    | [a]       -> pp out a
    | a::lst    -> fprintf out "%a%s" (pp_list sep pp) lst sep

let rec pp_pterm out = function
  | PreType _        -> output_string out "Type"
  | PreId (_,v)      -> pp_ident out v
  | PreQId (_,m,v)   -> fprintf out "%a.%a" pp_ident m pp_ident v
  | PreApp (lst)     -> pp_list " " pp_pterm_wp  out lst
  | PreLam (_,v,a,b) -> fprintf out "%a:%a => %a" pp_ident v pp_pterm_wp a pp_pterm b
  | PrePi (o,a,b)    ->
      ( match o with 
          | None       -> fprintf out "%a -> %a" pp_pterm_wp a pp_pterm b
          | Some (_,v) -> fprintf out "%a:%a -> %a" pp_ident v pp_pterm_wp a pp_pterm b )

and pp_pterm_wp out = function
  | PreType _ | PreId _ | PreQId _ as t  -> pp_pterm out t
  | t                                    -> fprintf out "(%a)" pp_pterm t

let pp_pconst out = function
    | ( None , id )     -> pp_ident out id
    | ( Some md , id )  -> fprintf out "%a.%a" pp_ident md pp_ident id

let rec pp_ppattern out = function
  | Unknown _                   -> output_string out "_"
  | PPattern (_,md,id,[])       -> pp_pconst out (md,id)
  | PPattern (_,md,id,lst)      ->
      fprintf out "(%a %a)" pp_pconst (md,id) (pp_list " " pp_ppattern) lst

let pp_const out (m,v) =
  if ident_eq m !Global.name then pp_ident out v
  else fprintf out "%a.%a" pp_ident m pp_ident v

let rec pp_term out = function
  | Kind                -> output_string out "Kind"
  | Type                -> output_string out "Type"
  | Meta n when !Global.debug_level > 0 -> fprintf out "?[%i]" n 
  | Meta n              -> output_string out "_"
  | DB  (x,n) when !Global.debug_level > 0 -> fprintf out "%a[%i]" pp_ident x n 
  | DB  (x,n)           -> pp_ident out x
  | Const (m,v)         -> pp_const out (m,v)
  | App args            -> pp_list " " pp_term_wp out args
  | Lam (x,a,f)         -> fprintf out "%a:%a => %a" pp_ident x pp_term_wp a pp_term f
  | Pi  (None,a,b)      -> fprintf out "%a -> %a" pp_term_wp a pp_term b
  | Pi  (Some x,a,b)    -> fprintf out "%a:%a -> %a" pp_ident x pp_term_wp a pp_term b

and pp_term_wp out = function
  | Kind | Type  | DB _ | Const _ as t -> pp_term out t
  | t                                  -> fprintf out "(%a)" pp_term t 

let rec pp_pattern_sub s out = function
  | Var (Some id,v)          -> fprintf out "%a[%i]" pp_ident id v
  | Var (None, n)             ->
      ( try let t = (List.assoc n s) in fprintf out "{%a}" pp_term t
        with Not_found -> fprintf out "_[%i]" n )
  | Pattern (m,v,arr)   ->
      if Array.length arr = 0 then pp_const out (m,v)
      else fprintf out "%a %a" pp_const (m,v) (pp_list " " (pp_pattern_wp s)) (Array.to_list arr)

and pp_pattern_wp s out = function
  | Pattern (_,_,args) as p when (Array.length args != 0) -> fprintf out "(%a)" (pp_pattern_sub s) p 
  |  p -> pp_pattern_sub s out p

let pp_pattern = pp_pattern_sub []

let pp_context out ctx =
  pp_list ".\n" (
    fun out (x,ty) ->
      if ident_eq empty x then fprintf out "?: %a" pp_term ty
      else fprintf out "%a: %a" pp_ident x pp_term ty )
    out (List.rev ctx)


let pp_rule out r =
  fprintf out "[%i] %a --> %a" r.nb 
    (pp_pattern_sub r.sub) (Pattern (!Global.name,r.id,r.args))
    pp_term r.ri
(*
 let string_of_cpair cp =
 let pos = String.concat "." (List.map string_of_int cp.pos) in
 "Rule (" ^ string_of_int cp.rule1 ^ ") and (" ^ string_of_int cp.rule2 ^
 ") overlap at pos:" ^ pos ^" \n"
 ^ string_of_pattern cp.root ^ " --> " ^ string_of_term cp.red1 ^ "\n"
 ^ string_of_pattern cp.root ^ " --> " ^ string_of_term cp.red2 ^ "\n"
 ^ "Joinability: " ^ (if cp.joinable then "OK" else "KO")
 *) 

let tab t = String.make (t*4) ' '

let rec pp_gdt0 t out = function
  | Test ([],te,_)              -> pp_term out te
  | Test (lst,te,def)           ->
      let tab = tab t in
      let aux out (i,j) = fprintf out "%a=%a" pp_term i pp_term j in
        fprintf out "\n%sif %a then %a\n%selse %a" tab (pp_list " and " aux) lst
          pp_term te tab (pp_def (t+1)) def
  | Switch (i,cases,def)      ->
      let tab = tab t in
      let pp_case out (mv,g) = 
        fprintf out "\n%sif $%i=%a then %a" tab i pp_const mv (pp_gdt0 (t+1)) g
      in
        fprintf out "%a\n%sdefault: %a" (pp_list "" pp_case) 
          cases tab (pp_def (t+1)) def
      
and pp_def t out = function
  | None        -> output_string out "FAIL"
  | Some g      -> pp_gdt0 t out g

let pp_gdt out (m,v,i,g) =
  fprintf out "GDT for '%a' with %i argument(s): %a" 
    pp_const (m,v) i (pp_gdt0 0) g 
