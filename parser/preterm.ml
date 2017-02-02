open Basic

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm option * preterm
  | PrePi   of loc * ident option * preterm * preterm

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list
  | PLambda     of loc*ident*prepattern
  | PJoker      of loc

type param =
  | PDecl of loc * ident * preterm
  | PDef of loc * ident * preterm
type pdecl      = loc * ident
type pfield = loc * ident * preterm
type pcontext   = pdecl list
type prule      = loc * pdecl list * ident option * ident * prepattern list * preterm

open Printf

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
