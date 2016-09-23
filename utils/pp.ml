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

(* Idents generated from underscores by the parser start with a question mark.
   We have sometimes to avoid to print them because they are not valid tokens. *)
let is_dummy_ident i = (string_of_ident i).[0] = '?'
let is_regular_ident i = (string_of_ident i).[0] <> '?'

let print_db out (x,n) =
  if !print_db_enabled then Format.fprintf out "%a[%i]" print_ident x n
  else print_ident out x

let print_db_or_underscore out (x,n) =
  if is_dummy_ident x then Format.fprintf out "_"
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

let fresh_name names base =
  if List.mem base names then
    let i = ref 0 in
    let name i = hstring (string_of_ident base ^ string_of_int i) in
    while List.mem (name !i) names do
      incr i
    done;
    name !i
  else base

let rec subst map = function
  | DB (_,x,_) as t when is_dummy_ident x -> t
  | DB (l,x,n) as t ->
     begin
       try
         let newname = List.nth map n in
         mk_DB l newname n
       with Failure "nth" -> t
     end
  | Kind
  | Type _
  | Const _ as t       -> t
  | App (f,a,args)     -> mk_App (subst map f)
                                (subst map a)
                                (List.map (subst map) args)
  | Lam (l,x,None,f)   -> let x' = fresh_name map x in
                         mk_Lam l x' None (subst (x' :: map) f)
  | Lam (l,x,Some a,f) -> let x' = fresh_name map x in
                         mk_Lam l x' (Some (subst map a)) (subst (x' :: map) f)
  | Pi  (l,x,a,b)      -> let x' =
                           if is_dummy_ident x then x else fresh_name map x
                         in
                         mk_Pi l x' (subst map a) (subst (x' :: map) b)


let rec print_raw_term out = function
  | Kind               -> Format.pp_print_string out "Kind"
  | Type _             -> Format.pp_print_string out "Type"
  | DB  (_,x,n)        -> print_db out (x,n)
  | Const (_,m,v)      ->
    let module_sep = Str.regexp "*///*" in
    if !resugar then
      print_const out (m,hstring (Str.global_replace module_sep "." (string_of_ident v)))
    else
      print_const out (m,v)
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

(* Overwrite print_term by a name-clash avoiding version *)
let print_term out t = print_term out (subst [] t)

let print_bv out (_,id,i) = print_db out (id,i)

let rec print_pattern out = function
  | Var (_,id,i,[]) -> print_db_or_underscore out (id,i)
  | Var (_,id,i,lst)     -> Format.fprintf out "%a %a" print_db_or_underscore (id,i) (print_list " " print_pattern_wp) lst
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
    (print_list ", " print_decl) (List.filter (fun (_, id) -> is_regular_ident id) ctx)
    print_pattern pat
    print_term te

let print_rule2 out (ctx,pat,te) =
  let print_decl out (_,id,ty) =
    Format.fprintf out "@[<hv>%a:@,%a@]" print_ident id print_term ty
  in
  Format.fprintf out
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) (List.rev ctx)
    print_pattern pat
    print_term te

let print_frule out r = print_rule2 out (r.ctx,Pattern(r.l,r.md,r.id,r.args),r.rhs)
