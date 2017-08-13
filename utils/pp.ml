open Basic
open Term
open Rule
open Printf

(* FIXME: this module is highly redondant with printing functions insides kernel modules *)

(* TODO: make that debuging functions returns a string *)
let print_db_enabled = ref false
let print_default = ref false
let name () = Env.get_name ()

let print_ident fmt id = Format.pp_print_string fmt (string_of_ident id)

let rec print_list sep pp out = function
    | []        -> ()
    | [a]       -> pp out a
    | a::lst    ->
        Format.fprintf out "%a%s@,%a" pp a sep (print_list sep pp) lst

let print_const out (m,v) =
  if ident_eq m (name ()) then print_ident out v
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
     if List.mem v map && ident_eq (name ()) m then
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

let print_typed_context fmt ctx =
  print_list ".\n"
    (fun out (_,x,ty) ->
      Format.fprintf fmt "@[<hv>%a:@ %a@]" print_ident x print_term ty
    ) fmt (List.rev ctx)

let print_rule_name fmt rule =
  let aux b md id =
    if b || !print_default then
      if ident_eq md (Env.get_name ()) then
        Format.fprintf fmt "@[<h>{%s}@] " (string_of_ident id)
      else
      Format.fprintf fmt "@[<h>{%s.%s}@] " (string_of_ident md) (string_of_ident id)
    else
      Format.fprintf fmt ""
  in
    match rule with
      | Delta(md,id) -> aux true md id (* not printed *)
    | Gamma(b,md,id) -> aux b md id

let print_untyped_rule fmt (rule:untyped_rule) =
  let print_decl out (_,id) =
    Format.fprintf out "@[<hv>%a@]" print_ident id
  in
  Format.fprintf fmt
    "@[<hov2>%a@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    print_rule_name rule.name
    (print_list ", " print_decl) (List.filter (fun (_, id) -> is_regular_ident id) rule.ctx)
    print_pattern rule.pat
    print_term rule.rhs

let print_typed_rule out (rule:typed_rule) =
  let print_decl out (_,id,ty) =
    Format.fprintf out "@[<hv>%a:@,%a@]" print_ident id print_term ty
  in
  Format.fprintf out
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) rule.ctx
    print_pattern rule.pat
    print_term rule.rhs

let print_rule_infos out ri =
  let rule = { name = ri.name ;
               ctx = ri.ctx ;
               pat =  pattern_of_rule_infos ri;
               rhs = ri.rhs
             }
  in
  print_typed_rule out rule
