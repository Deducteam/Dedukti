open Basic
open Term
open Rule

(* FIXME: this module is highly redondant with printing functions insides kernel modules *)

(* TODO: make that debuging functions returns a string *)
let print_db_enabled = ref false
let print_default = ref false
let name () = Env.get_name ()

let rec print_list sep pp fmt = function
    | []        -> ()
    | [a]       -> pp fmt a
    | a::lst    ->
        Format.fprintf fmt "%a%s@,%a" pp a sep (print_list sep pp) lst

let print_ident = pp_ident

let print_mident = pp_mident

let print_name = pp_name

let print_const fmt cst =
  let md = md cst in
  if mident_eq md (name ()) then print_ident fmt (id cst)
  else Format.fprintf fmt "%a" pp_name cst

(* Idents generated from underscores by the parser start with a question mark.
   We have sometimes to avoid to print them because they are not valid tokens. *)
let is_dummy_ident i = (string_of_ident i).[0] = '?'
let is_regular_ident i = (string_of_ident i).[0] <> '?'

let print_db fmt (x,n) =
  if !print_db_enabled then Format.fprintf fmt "%a[%i]" print_ident x n
  else print_ident fmt x

let print_db_or_underscore fmt (x,n) =
  if is_dummy_ident x then Format.fprintf fmt "_"
  else print_ident fmt x

let fresh_name names base =
  if List.mem base names then
    let i = ref 0 in
    let name i = mk_ident (string_of_ident base ^ string_of_int i) in
    while List.mem (name !i) names do
      incr i
    done;
    name !i
  else base

let rec subst map = function
  | Meta(l,x,n,mt) as t -> t
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
  | Const (l,cst) as t       ->
    let m,v = md cst, id cst in
    if List.mem v map && mident_eq (name ()) m then
      let v' = (mk_ident ((string_of_mident m) ^ "." ^ (string_of_ident v))) in
       mk_Const l (mk_name m v')
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


let rec print_term fmt = function
  | Kind               -> Format.pp_print_string fmt "Kind"
  | Type _             -> Format.pp_print_string fmt "Type"
  | DB  (_,x,n)        -> print_db fmt (x,n)
  | Meta  (_,x,n,mt) ->
    begin
      match !mt with
      | None -> Format.fprintf fmt "?%a[%d]" pp_ident x n
      | Some te -> Format.fprintf fmt "%a" pp_term te
    end
  | Const (_,cst)      -> print_const fmt cst
  | App (f,a,args)     ->
      Format.fprintf fmt "@[<hov2>%a@]" (print_list " " print_term_wp) (f::a::args)
  | Lam (_,x,None,f)   -> Format.fprintf fmt "@[%a =>@ @[%a@]@]" print_ident x print_term f
  | Lam (_,x,Some a,f) ->
      Format.fprintf fmt "@[%a:@,%a =>@ @[%a@]@]" print_ident x print_term_wp a print_term f
  | Pi  (_,x,a,b) when ident_eq x qmark  ->
      (* arrow, no pi *)
      Format.fprintf fmt "@[%a ->@ @[%a@]@]" print_term_wp a print_term b
  | Pi  (_,x,a,b)      ->
      Format.fprintf fmt "@[%a:%a ->@ @[%a@]@]" print_ident x print_term_wp a print_term b

and print_term_wp fmt = function
  | Kind | Type _ | DB _ | Const _ as t -> print_term fmt t
  | t                                  -> Format.fprintf fmt "(%a)" print_term t

(* Overwrite print_term by a name-clash avoiding version *)
let print_term fmt t = print_term fmt (subst [] t)

let print_bv fmt (_,id,i) = print_db fmt (id,i)

let rec print_pattern fmt = function
  | Var (_,id,i,[]) -> print_db_or_underscore fmt (id,i)
  | Var (_,id,i,lst)     -> Format.fprintf fmt "%a %a" print_db_or_underscore (id,i) (print_list " " print_pattern_wp) lst
  | Brackets t           -> Format.fprintf fmt "{ %a }" print_term t
  | Pattern (_,cst,[])   -> Format.fprintf fmt "%a" print_const cst
  | Pattern (_,cst,pats) -> Format.fprintf fmt "%a %a" print_const cst (print_list " " print_pattern_wp) pats
  | Lambda (_,x,p)       -> Format.fprintf fmt "@[%a => %a@]" print_ident x print_pattern p
and print_pattern_wp fmt = function
  | Pattern _ | Lambda _ as p -> Format.fprintf fmt "(%a)" print_pattern p
  | p -> print_pattern fmt p

let print_typed_context fmt ctx =
  print_list ".\n"
    (fun fmt (_,x,ty) ->
      Format.fprintf fmt "@[<hv>%a:@ %a@]" print_ident x print_term ty
    ) fmt (List.rev ctx)

let print_rule_name fmt rule =
  let aux b cst =
    if b || !print_default then
      if mident_eq (md cst) (Env.get_name ()) then
        Format.fprintf fmt "@[<h>{%a}@] " print_ident (id cst)
      else
      Format.fprintf fmt "@[<h>{%a}@] " print_name cst
    else
      Format.fprintf fmt ""
  in
    match rule with
      | Delta(cst) -> aux true cst (* not printed *)
    | Gamma(b,cst) -> aux b cst

let print_untyped_rule fmt (rule:untyped_rule) =
  let print_decl fmt (_,id) =
    Format.fprintf fmt "@[<hv>%a@]" print_ident id
  in
  Format.fprintf fmt
    "@[<hov2>%a@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    print_rule_name rule.name
    (print_list ", " print_decl) (List.filter (fun (_, id) -> is_regular_ident id) rule.ctx)
    print_pattern rule.pat
    print_term rule.rhs

let print_typed_rule fmt (rule:typed_rule) =
  let print_decl fmt (_,id,ty) =
    Format.fprintf fmt "@[<hv>%a:@,%a@]" print_ident id print_term ty
  in
  Format.fprintf fmt
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) rule.ctx
    print_pattern rule.pat
    print_term rule.rhs

let print_rule_infos fmt ri =
  let rule = { name = ri.name ;
               ctx = ri.ctx ;
               pat =  pattern_of_rule_infos ri;
               rhs = ri.rhs
             }
  in
  print_typed_rule fmt rule
