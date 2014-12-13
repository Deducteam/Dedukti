open Basics
open Preterm
open Term
open Rule
open Printf

let print_db = ref false
let name = ref qmark

let rec pp_list sep pp out = function
    | []        -> ()
    | [a]       -> pp out a
    | a::lst    -> fprintf out "%a%s%a" pp a sep (pp_list sep pp) lst

let rec pp_pterm out = function
  | PreType _        -> output_string out "Type"
  | PreId (_,v)      -> pp_ident out v
  | PreQId (_,m,v)   -> fprintf out "%a.%a" pp_ident m pp_ident v
  | PreApp (f,a,lst) -> pp_list " " pp_pterm_wp  out (f::a::lst)
  | PreLam (_,v,None,b) -> fprintf out "%a => %a" pp_ident v pp_pterm b
  | PreLam (_,v,Some a,b) -> fprintf out "%a:%a => %a" pp_ident v pp_pterm_wp a pp_pterm b
  | PrePi (_,o,a,b)    ->
      ( match o with
          | None   -> fprintf out "%a -> %a" pp_pterm_wp a pp_pterm b
          | Some v -> fprintf out "%a:%a -> %a" pp_ident v pp_pterm_wp a pp_pterm b )

and pp_pterm_wp out = function
  | PreType _ | PreId _ | PreQId _ as t  -> pp_pterm out t
  | t                                    -> fprintf out "(%a)" pp_pterm t

let pp_pconst out = function
    | ( None , id )     -> pp_ident out id
    | ( Some md , id )  -> fprintf out "%a.%a" pp_ident md pp_ident id

let rec pp_ppattern out = function
  | PPattern (_,md,id,[])       -> pp_pconst out (md,id)
  | PPattern (_,md,id,lst)      ->
      fprintf out "%a %a" pp_pconst (md,id) (pp_list " " pp_ppattern) lst
  | PCondition pte              -> fprintf out "{ %a }" pp_pterm pte
  | PJoker _                    -> fprintf out "_"
  | PLambda (_,id,p)            -> fprintf out "%a => %a" pp_ident id pp_ppattern p
and pp_ppattern_wp out = function
  | PLambda (_,_,_)
  | PPattern (_,_,_,_::_) as p  -> fprintf out "(%a)" pp_ppattern p
  | p                           -> pp_ppattern out p

let pp_const out (m,v) =
  if ident_eq m !name then pp_ident out v
  else fprintf out "%a.%a" pp_ident m pp_ident v

let pp_db out (x,n) =
  if !print_db then fprintf out "%a[%i]" pp_ident x n
  else pp_ident out x

let rec pp_term out = function
  | Kind               -> output_string out "Kind"
  | Type _             -> output_string out "Type"
  | DB  (_,x,n)        -> pp_db out (x,n)
  | Const (_,m,v)      -> pp_const out (m,v)
  | App (f,a,args)     -> pp_list " " pp_term_wp out (f::a::args)
  | Lam (_,x,None,f)   -> fprintf out "%a => %a" pp_ident x pp_term f
  | Lam (_,x,Some a,f) -> fprintf out "%a:%a => %a" pp_ident x pp_term_wp a pp_term f
  | Pi  (_,x,a,b)      -> fprintf out "%a:%a -> %a" pp_ident x pp_term_wp a pp_term b

and pp_term_wp out = function
  | Kind | Type _ | DB _ | Const _ as t -> pp_term out t
  | t                                  -> fprintf out "(%a)" pp_term t

let pp_bv out (_,id,i) = pp_db out (id,i)

let rec pp_pattern out = function
  | Var (_,id,i,[]) -> pp_db out (id,i)
  | Var (_,id,i,lst)    -> fprintf out "%a %a" pp_db (id,i) (pp_list " " pp_pattern_wp) lst
  | Brackets t           -> fprintf out "{ %a }" pp_term t
  | Pattern (_,m,v,[])   -> fprintf out "%a" pp_const (m,v)
  | Pattern (_,m,v,pats) -> fprintf out "%a %a" pp_const (m,v) (pp_list " " pp_pattern_wp) pats
  | Lambda (_,x,p)       -> fprintf out "%a => %a" pp_ident x pp_pattern p
and pp_pattern_wp out = function
  | Pattern _ | Lambda _ as p -> fprintf out "(%a)" pp_pattern p
  | p -> pp_pattern out p

let pp_context out ctx =
  pp_list ".\n" (fun out (_,x,ty) -> fprintf out "%a: %a" pp_ident x pp_term ty )
    out (List.rev ctx)

let pp_rule out (ctx,pat,te) =
  let pp_decl out (_,id,ty) = fprintf out "%a:%a" pp_ident id pp_term ty in
    fprintf out "[%a] %a --> %a"
      (pp_list "," pp_decl) ctx
      pp_pattern pat
      pp_term te

let pp_frule out r = pp_rule out (r.ctx,Pattern(r.l,r.md,r.id,r.args),r.rhs)

let tab t = String.make (t*4) ' '

let pp_pc out = function
  | Syntactic _ -> fprintf out "Sy"
  | MillerPattern _ -> fprintf out "Mi"

let rec pp_dtree t out = function
  | Test (pc,[],te,None)   -> fprintf out "(%a) %a" pp_pc pc pp_term te
  | Test (_,[],_,def)      -> assert false
  | Test (pc,lst,te,def)  ->
      let tab = tab t in
      let aux out (i,j) = fprintf out "%a=%a" pp_term i pp_term j in
        fprintf out "\n%sif %a then (%a) %a\n%selse (%a) %a" tab (pp_list " and " aux) lst
          pp_pc pc pp_term te tab pp_pc pc (pp_def (t+1)) def
  | Switch (i,cases,def)->
      let tab = tab t in
      let pp_case out = function
        | CConst (_,m,v), g ->
            fprintf out "\n%sif $%i=%a then %a" tab i pp_const (m,v) (pp_dtree (t+1)) g
        | CLam, g -> fprintf out "\n%sif $%i=Lambda then %a" tab i (pp_dtree (t+1)) g
        | CDB (_,n), g -> fprintf out "\n%sif $%i=DB[%i] then %a" tab i n (pp_dtree (t+1)) g
      in
        fprintf out "%a\n%sdefault: %a" (pp_list "" pp_case)
          cases tab (pp_def (t+1)) def

and pp_def t out = function
  | None        -> output_string out "FAIL"
  | Some g      -> pp_dtree t out g

let pp_rw out (m,v,i,g) =
  fprintf out "GDT for '%a' with %i argument(s): %a"
    pp_const (m,v) i (pp_dtree 0) g
