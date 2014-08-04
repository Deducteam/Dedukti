open Types
open Printf

let rec pp_list sep pp out = function
    | []        -> ()
    | [a]       -> pp out a
    | a::lst    -> fprintf out "%a%s%a" pp a sep (pp_list sep pp) lst

let rec pp_pterm out = function
  | PreType _        -> output_string out "Type"
  | PreId (_,v)      -> pp_ident out v
  | PreQId (_,m,v)   -> fprintf out "%a.%a" pp_ident m pp_ident v
  | PreApp (f,a,lst) -> pp_list " " pp_pterm_wp  out (f::a::lst)
  | PreLam (_,v,a,b) -> fprintf out "%a:%a => %a" pp_ident v pp_pterm_wp a pp_pterm b
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
and pp_ppattern_wp out = function
  | PPattern (_,_,_,_::_) as p  -> fprintf out "(%a)" pp_ppattern p
  | p                           -> pp_ppattern out p

let pp_const out (m,v) =
  if ident_eq m !Global.name then pp_ident out v
  else fprintf out "%a.%a" pp_ident m pp_ident v

let rec pp_term out = function
  | Kind                -> output_string out "Kind"
  | Type _               -> output_string out "Type"
  | Meta (_,n) when !Global.debug_level > 0 -> fprintf out "?[%i]" n
  | Meta (_,n)              -> output_string out "_"
  | Var  (_,v) when !Global.debug_level > 0 -> Var.pp out v
  | Var  (_,v)                              -> pp_ident out (Var.ident v)
  | Const (_,m,v)         -> pp_const out (m,v)
  | App (f,a,args)      -> pp_list " " pp_term_wp out (f::a::args)
  | Lam (_,v,a,f)         -> fprintf out "%a:%a => %a" Var.pp v pp_term_wp a pp_term f
  | Pi  (_,None,a,b)      -> fprintf out "%a -> %a" pp_term_wp a pp_term b
  | Pi  (_,Some v,a,b)    -> fprintf out "%a:%a -> %a" Var.pp v pp_term_wp a pp_term b

and pp_term_wp out = function
  | Kind | Type _ | Var _ | Const _ as t  -> pp_term out t
  | t                                     -> fprintf out "(%a)" pp_term t

let rec pp_pattern out = function
  | Var (_,v)        ->
      if !Global.debug_level > 0 then Var.pp out v
      else pp_ident out (Var.ident v)
  | Brackets t        -> fprintf out "{ %a }" pp_term t
  | Pattern (_,m,v,[])  -> fprintf out "%a" pp_const (m,v)
  | Pattern (_,m,v,pats) ->
      fprintf out "%a %a" pp_const (m,v) (pp_list " " pp_pattern_wp) pats
  | Joker _                    -> fprintf out "_"
and pp_pattern_wp out = function
  | Pattern (_,_,_,_) as p -> fprintf out "(%a)" pp_pattern p
  | p -> pp_pattern out p

let pp_subst ~sep out sigma =
  pp_list sep
    (fun out (v,t) -> fprintf out "%a: %a" Var.pp v pp_term t)
    out (VarMap.bindings sigma)

let pp_subst_l ~sep out sigma =
  pp_list sep
    (fun out (v, lazy t) -> fprintf out "%a: %a" Var.pp v pp_term t)
    out (VarMap.bindings sigma)

let pp_context out ctx =
  pp_list ".\n" (
    fun out (x,ty) ->
      if ident_eq empty x then fprintf out "?: %a" pp_term ty
      else fprintf out "%a: %a" pp_ident x pp_term ty )
    out (IdentMap.bindings ctx.const2ty);
  output_char out '\n';
  pp_subst ~sep:".\n" out ctx.var2ty

let pp_rule out r =
  let pp_decl out (x,ty) = fprintf out "%a:%a" pp_ident x pp_term ty in
    fprintf out "[%a] %a --> %a"
      (pp_list "," pp_decl) (IdentMap.bindings r.ctx.const2ty)
      pp_pattern (Pattern (dloc,r.md,r.id,r.args))
      pp_term r.rhs

let tab t = String.make (t*4) ' '

let rec pp_dtree t out = function
  | Test (_,[],te,None)   -> pp_term out te
  | Test (_,[],te,_)      -> assert false
  | Test (_,lst,te,def)   ->
      let tab = tab t in
      let aux out (EqTerm(i,j)) = fprintf out "%a=%a" pp_term i pp_term j in
      fprintf out "\n%sif %a then %a\n%selse %a" tab (pp_list " and " aux) lst
        pp_term te tab (pp_def (t+1)) def
  | Switch (i,cases,def)->
      let tab = tab t in
      let pp_case out (_,m,v,g) =
        fprintf out "\n%sif $%i=%a then %a" tab i
          pp_const (m,v) (pp_dtree (t+1)) g
      in
        fprintf out "%a\n%sdefault: %a" (pp_list "" pp_case)
          cases tab (pp_def (t+1)) def

and pp_def t out = function
  | None        -> output_string out "FAIL"
  | Some g      -> pp_dtree t out g

let pp_rw out (m,v,i,g) =
  fprintf out "GDT for '%a' with %i argument(s): %a"
    pp_const (m,v) i (pp_dtree 0) g
