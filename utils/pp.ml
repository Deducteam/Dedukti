open Basics
open Preterm
open Term
open Rule
open Printf

let print_db_enabled = ref false
let name = ref qmark

let rec print_list sep pp out = function
    | []        -> ()
    | [a]       -> pp out a
    | a::lst    ->
        Format.fprintf out "%a%s@,%a" pp a sep (print_list sep pp) lst

let rec print_pterm out = function
  | PreType _        -> Format.pp_print_string out "Type"
  | PreId (_,v)      -> print_ident out v
  | PreQId (_,m,v)   -> Format.fprintf out "%a.%a" print_ident m print_ident v
  | PreApp (f,a,lst) -> print_list " " print_pterm_wp  out (f::a::lst)
  | PreLam (_,v,None,b) -> Format.fprintf out "%a => %a" print_ident v print_pterm b
  | PreLam (_,v,Some a,b) -> Format.fprintf out "%a:%a => %a" print_ident v print_pterm_wp a print_pterm b
  | PrePi (_,o,a,b)    ->
      match o with
      | None   ->
          Format.fprintf out "%a -> %a" print_pterm_wp a print_pterm b
      | Some v ->
          Format.fprintf out "%a:%a -> %a" print_ident v print_pterm_wp a print_pterm b

and print_pterm_wp out = function
  | PreType _ | PreId _ | PreQId _ as t  -> print_pterm out t
  | t                                    -> Format.fprintf out "(%a)" print_pterm t

let print_pconst out = function
    | ( None , id )     -> print_ident out id
    | ( Some md , id )  -> Format.fprintf out "%a.%a" print_ident md print_ident id

let print_const out (m,v) =
  if ident_eq m !name then print_ident out v
  else Format.fprintf out "%a.%a" print_ident m print_ident v

let print_db out (x,n) =
  if !print_db_enabled then Format.fprintf out "%a[%i]" print_ident x n
  else print_ident out x

let rec print_ppattern out = function
  | PPattern (_,md,id,[])       -> print_pconst out (md,id)
  | PPattern (_,md,id,lst)      ->
      Format.fprintf out "%a %a" print_pconst (md,id) (print_list " " print_ppattern) lst
  | PCondition pte              -> Format.fprintf out "{ %a }" print_pterm pte
  | PJoker _                    -> Format.fprintf out "_"
  | PLambda (_,id,p)            -> Format.fprintf out "%a => %a" print_ident id print_ppattern p
and print_ppattern_wp out = function
  | PLambda (_,_,_)
  | PPattern (_,_,_,_::_) as p  -> Format.fprintf out "(%a)" print_ppattern p
  | p                           -> print_ppattern out p

let rec print_term out = function
  | Kind               -> Format.pp_print_string out "Kind"
  | Type _             -> Format.pp_print_string out "Type"
  | DB  (_,x,n)        -> print_db out (x,n)
  | Const (_,m,v)      -> print_const out (m,v)
  | App (f,a,args)     ->
      Format.fprintf out "@[<hov2>%a@]" (print_list " " print_term_wp) (f::a::args)
  | Lam (_,x,None,f)   -> Format.fprintf out "@[%a =>@ @[%a@]@]" print_ident x print_term f
  | Lam (_,x,Some a,f) ->
      Format.fprintf out "@[%a:@,%a =>@ @[%a@]@]" print_ident x print_term_wp a print_term f
  | Pi  (_,x,a,b) when ident_eq x qmark  ->
      (* arrow, no pi *)
      Format.fprintf out "@[%a ->@ @[%a@]@]" print_term_wp a print_term b
  | Pi  (_,x,a,b)      ->
      Format.fprintf out "@[%a:%a ->@ @[%a@]@]" print_ident x print_term_wp a print_term b

and print_term_wp out = function
  | Kind | Type _ | DB _ | Const _ as t -> print_term out t
  | t                                  -> Format.fprintf out "(%a)" print_term t

let print_bv out (_,id,i) = print_db out (id,i)

let rec print_pattern out = function
  | Var (_,id,i,[]) -> print_db out (id,i)
  | Var (_,id,i,lst)     -> Format.fprintf out "%a %a" print_db (id,i) (print_list " " print_pattern_wp) lst
  | Brackets t           -> Format.fprintf out "{ %a }" print_term t
  | Pattern (_,m,v,[])   -> Format.fprintf out "%a" print_const (m,v)
  | Pattern (_,m,v,pats) -> Format.fprintf out "%a %a" print_const (m,v) (print_list " " print_pattern_wp) pats
  | Lambda (_,x,p)       -> Format.fprintf out "@[%a => %a@]" print_ident x print_pattern p
and print_pattern_wp out = function
  | Pattern _ | Lambda _ as p -> Format.fprintf out "(%a)" print_pattern p
  | p -> print_pattern out p

let print_context out ctx =
  print_list ".\n"
    (fun out (_,x,ty) ->
      Format.fprintf out "@[<hv>%a:@ %a@]" print_ident x print_term ty
    ) out (List.rev ctx)

let print_rule out (ctx,pat,te) =
  let print_decl out (_,id) =
    Format.fprintf out "@[<hv>%a@]" print_ident id
  in
  Format.fprintf out
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) ctx
    print_pattern pat
    print_term te

let print_rule2 out (ctx,pat,te) =
  let print_decl out (_,id,ty) =
    Format.fprintf out "@[<hv>%a:@,%a@]" print_ident id print_term ty
  in
  Format.fprintf out
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) ctx
    print_pattern pat
    print_term te

let print_frule out r = print_rule2 out (r.ctx,Pattern(r.l,r.md,r.id,r.args),r.rhs)
