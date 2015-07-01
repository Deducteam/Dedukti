open Basics
open Preterm
open Term
open Rule
open Printf

let print_db_enabled = ref false
let name = ref qmark
let resugar = ref true

let rec print_list sep pp out = function
    | []        -> ()
    | [a]       -> pp out a
    | a::lst    ->
        Format.fprintf out "%a%s@,%a" pp a sep (print_list sep pp) lst

let rec pp_pterm out = function
  | PreType _        -> output_string out "Type"
  | PreId (_,v)      -> pp_ident out v
  | PreQId (_,m,v) when ident_eq m Builtins.modname -> pp_ident out v
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

let pp_pconst out = function
    | ( None , id )     -> pp_ident out id
    | ( Some md , id ) when ident_eq md Builtins.modname -> pp_ident out id
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

let pp_const out (m,v) =
  if ident_eq m !name || ident_eq m Builtins.modname then pp_ident out v
  else fprintf out "%a.%a" pp_ident m pp_ident v

let pp_db out (x,n) =
  if !print_db_enabled then fprintf out "%a[%i]" pp_ident x n
  else pp_ident out x

let rec pp_raw_term out = function
  | Kind               -> output_string out "Kind"
  | Type _             -> output_string out "Type"
  | DB  (_,x,n)        -> pp_db out (x,n)
  | Const (_,m,v)      -> pp_const out (m,v)
  | App (f,a,args)     -> pp_list " " pp_term_wp out (f::a::args)
  | Lam (_,x,None,f)   -> fprintf out "%a => %a" pp_ident x pp_term f
  | Lam (_,x,Some a,f) -> fprintf out "%a:%a => %a" pp_ident x pp_term_wp a pp_term f
  | Pi  (_,x,a,b)      -> fprintf out "%a:%a -> %a" pp_ident x pp_term_wp a pp_term b

and pp_term out t =
  try Builtins.pp_term out t
  with Builtins.Not_atomic_builtin ->
    pp_raw_term out t

and pp_raw_term_wp out = function
  | Kind | Type _ | DB _ | Const _ as t -> pp_raw_term out t
  | t                                  -> fprintf out "(%a)" pp_raw_term t

and pp_term_wp out t =
  try Builtins.pp_term out t
  with Builtins.Not_atomic_builtin ->
    pp_raw_term_wp out t

let print_const out (m,v) =
  if ident_eq m !name then print_ident out v
  else Format.fprintf out "%a.%a" print_ident m print_ident v

let print_db out (x,n) =
  if !print_db_enabled then Format.fprintf out "%a[%i]" print_ident x n
  else print_ident out x

let rec print_raw_term out = function
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

and print_term out t =
  if not !resugar then print_raw_term out t
  else
    try Builtins.print_term out t
    with Builtins.Not_atomic_builtin ->
      print_raw_term out t

and print_term_wp out = function
  | Kind | Type _ | DB _ | Const _ as t -> print_term out t
  | t                                   -> Format.fprintf out "(%a)" print_term t

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
  let print_decl out (_,id,ty) =
    Format.fprintf out "@[<hv>%a:@,%a@]" print_ident id print_term ty
  in
  Format.fprintf out
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) (List.rev ctx)
    print_pattern pat
    print_term te

let print_frule out r = print_rule out (r.ctx,Pattern(r.l,r.md,r.id,r.args),r.rhs)

let tab t = String.make (t*4) ' '

let print_pc out = function
  | Syntactic _ -> Format.fprintf out "Sy"
  | MillerPattern _ -> Format.fprintf out "Mi"

let rec print_dtree t out = function
  | Test (pc,[],te,None)   -> Format.fprintf out "(%a) %a" print_pc pc print_term te
  | Test (_,[],_,def)      -> assert false
  | Test (pc,lst,te,def)  ->
      let tab = tab t in
      let aux out (i,j) = Format.fprintf out "%a=%a" print_term i print_term j in
        Format.fprintf out "\n%sif %a then (%a) %a\n%selse (%a) %a" tab (print_list " and " aux) lst
          print_pc pc print_term te tab print_pc pc (print_def (t+1)) def
  | Switch (i,cases,def)->
      let tab = tab t in
      let print_case out = function
        | CConst (_,m,v), g ->
            Format.fprintf out "\n%sif $%i=%a.%a then %a" tab i print_ident m print_ident v (print_dtree (t+1)) g
        | CLam, g -> Format.fprintf out "\n%sif $%i=Lambda then %a" tab i (print_dtree (t+1)) g
        | CDB (_,n), g -> Format.fprintf out "\n%sif $%i=DB[%i] then %a" tab i n (print_dtree (t+1)) g
      in
        Format.fprintf out "%a\n%sdefault: %a" (print_list "" print_case)
          cases tab (print_def (t+1)) def

and print_def t out = function
  | None        -> Format.fprintf out "FAIL"
  | Some g      -> print_dtree t out g

let print_rw out (m,v,i,g) =
  Format.fprintf out "GDT for '%a.%a' with %i argument(s): %a"
    print_ident m print_ident v i (print_dtree 0) g
