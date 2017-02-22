open Basic
open Preterm
open Term
open Rule
open Printf

(* TODO: make that debuging functions returns a string *)
let print_db_enabled = ref true
let name = ref qmark

let pp_ident out x = output_string out (string_of_ident x)

let rec pp_list sep pp out = function
    | []        -> ()
    | [a]       -> pp out a
    | a::lst    -> Printf.fprintf out "%a%s%a" pp a sep (pp_list sep pp) lst

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

let rec pp_term out te =
  match te with
  | Kind               -> output_string out "Kind"
  | Type _             -> output_string out "Type"
  | DB  (_,x,n)        -> Printf.fprintf out "%a[%i]" pp_ident x n
  | Const (_,m,v)      -> Printf.fprintf out "%a.%a" pp_ident m pp_ident v
  | App (f,a,args)     -> pp_list " " pp_term_wp out (f::a::args)
  | Lam (_,x,None,f)   -> Printf.fprintf out "%a => %a" pp_ident x pp_term f
  | Lam (_,x,Some a,f) -> Printf.fprintf out "%a:%a => %a" pp_ident x pp_term_wp a pp_term f
  | Pi  (_,x,a,b)      -> Printf.fprintf out "%a:%a -> %a" pp_ident x pp_term_wp a pp_term b

and pp_term_wp out te =
  match te with
  | Kind | Type _ | DB _ | Const _ as t -> pp_term out t
  | t                                  -> Printf.fprintf out "(%a)" pp_term t

let rec pp_pattern out = function
  | Var (_,x,n,[]) -> fprintf out "%a[%i]" pp_ident x n
  | Var (_,x,n,lst) -> fprintf out "%a[%i] %a" pp_ident x n (pp_list " " pp_pattern_wp) lst
  | Brackets t -> fprintf out "{ %a }" pp_term t
  | Pattern (_,m,v,[]) -> fprintf out "%a.%a" pp_ident m pp_ident v
  | Pattern (_,m,v,pats) -> fprintf out "%a.%a %a" pp_ident m pp_ident v (pp_list " " pp_pattern_wp) pats
  | Lambda (_,x,p) -> fprintf out "%a => %a" pp_ident x pp_pattern p
and pp_pattern_wp out = function
  | Pattern _ | Lambda _ as p -> fprintf out "(%a)" pp_pattern p
  | p -> pp_pattern out p

let pp_untyped_rule out (ctx,pat,te) =
   let pp_decl out (_,id) = pp_ident out id in
    fprintf out "[%a] %a --> %a"
      (pp_list "," pp_decl) (List.rev ctx)
      pp_pattern pat
      pp_term te

let pp_typed_rule out (ctx,pat,te) =
   let pp_decl out (_,id,ty) = fprintf out "%a:%a" pp_ident id pp_term ty in
    fprintf out "[%a] %a --> %a"
      (pp_list "," pp_decl) (List.rev ctx)
      pp_pattern pat
      pp_term te

let pp_rule_infos out r =
  pp_typed_rule out (r.ctx,Pattern(r.l,r.md,r.id,r.args),r.rhs)

let pp_typed_context out ctx =
  pp_list ".\n" (fun out (_,x,ty) ->
                   Printf.fprintf out "%a: %a" pp_ident x pp_term ty )
    out (List.rev ctx)

let pp_pc out = function
  | Syntactic _ -> fprintf out "Sy"
  | MillerPattern _ -> fprintf out "Mi"

(* TODO: put this inside dtree *)
let tab t = String.make (t*4) ' '

let rec pp_dtree t out = function
  | Test (pc,[],te,None)   -> fprintf out "(%a) %a" pp_pc pc pp_term te
  | Test (_,[],_,def)      -> assert false
  | Test (pc,lst,te,def)  ->
      let tab = tab t in
      let aux out = function
        | Linearity (i,j) -> fprintf out "%d =l %d" i j
        | Bracket (i,j) -> fprintf out "%a =b %a" pp_term (mk_DB dloc dmark i) pp_term j
      in
      fprintf out "\n%sif %a then (%a) %a\n%selse (%a) %a" tab (pp_list " and " aux) lst
        pp_pc pc pp_term te tab pp_pc pc (pp_def (t+1)) def
  | Switch (i,cases,def)->
      let tab = tab t in
      let pp_case out = function
        | CConst (_,m,v), g ->
            fprintf out "\n%sif $%i=%a.%a then %a" tab i pp_ident m pp_ident v (pp_dtree (t+1)) g
        | CLam, g -> fprintf out "\n%sif $%i=Lambda then %a" tab i (pp_dtree (t+1)) g
        | CDB (_,n), g -> fprintf out "\n%sif $%i=DB[%i] then %a" tab i n (pp_dtree (t+1)) g
      in
        fprintf out "%a\n%sdefault: %a" (pp_list "" pp_case)
          cases tab (pp_def (t+1)) def

and pp_def t out = function
  | None        -> output_string out "FAIL"
  | Some g      -> pp_dtree t out g

let pp_rw out (m,v,i,g) =
  fprintf out "GDT for '%a.%a' with %i argument(s): %a"
    pp_ident m pp_ident v i (pp_dtree 0) g

let print_ident fmt id = Format.pp_print_string fmt (string_of_ident id)

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
       with Failure _ -> t
     end
  | Kind
  | Type _ as t -> t
  (* if there is a local variable that have the same name as a top level constant,
        then the module has to be printed *)
  (* a hack proposed by Raphael Cauderlier *)
  | Const (l,m,v) as t       ->
     if List.mem v map && ident_eq !name m then
       mk_Const l m (hstring ((string_of_ident m) ^ "." ^ (string_of_ident v)))
     else
       t
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

let print_typed_context out ctx =
  print_list ".\n"
    (fun out (_,x,ty) ->
      Format.fprintf out "@[<hv>%a:@ %a@]" print_ident x print_term ty
    ) out (List.rev ctx)

let print_untyped_rule out (ctx,pat,te) =
  let print_decl out (_,id) =
    Format.fprintf out "@[<hv>%a@]" print_ident id
  in
  Format.fprintf out
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) (List.filter (fun (_, id) -> is_regular_ident id) ctx)
    print_pattern pat
    print_term te

let print_typed_rule out (ctx,pat,te) =
  let print_decl out (_,id,ty) =
    Format.fprintf out "@[<hv>%a:@,%a@]" print_ident id print_term ty
  in
  Format.fprintf out
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) ctx
    print_pattern pat
    print_term te

let print_rule_infos out r = print_typed_rule out (r.ctx,Pattern(r.l,r.md,r.id,r.args),r.rhs)
