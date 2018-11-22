open Basic
open Term
open Rule
open Entry
open Env
(* FIXME: this module is highly redondant with printing functions insides kernel modules *)

(* TODO: make that debuging functions returns a string *)
let print_db_enabled = ref false
let print_default_name = ref false

let cur_md = ref None
let get_module () =
  match !cur_md with
  | None -> get_name ()
  | Some md -> md

let set_module md = cur_md := Some md

let print_list = pp_list

let print_ident = pp_ident

let print_mident = pp_mident

let print_name = pp_name

let print_const out cst =
  let md = md cst in
  if mident_eq md (get_module ()) then print_ident out (id cst)
  else Format.fprintf out "%a" pp_name cst

(* Idents generated from underscores by the parser start with a question mark.
   We have sometimes to avoid to print them because they are not valid tokens. *)
let is_dummy_ident i = (string_of_ident i).[0] = '$'
let is_regular_ident i = (string_of_ident i).[0] <> '$'

let print_db out (x,n) =
  if !print_db_enabled then Format.fprintf out "%a[%i]" print_ident x n
  else print_ident out x

let print_db_or_underscore out (x,n) =
  if is_dummy_ident x then Format.fprintf out "_"
  else print_ident out x

let fresh_name names base =
  if List.mem base names then
    let i = ref 0 in
    let name i = mk_ident (string_of_ident base ^ string_of_int i) in
    while List.mem (name !i) names do
      incr i
    done;
    name !i
  else base

let rec subst ctx = function
  | DB (_,x,_) as t when is_dummy_ident x -> t
  | DB (l,x,n) as t -> ( try mk_DB l (List.nth ctx n) n with Failure _ -> t)
  | Kind
  | Type _ as t -> t
  (* if there is a local variable that have the same name as a top level constant,
        then the module has to be printed *)
  (* a hack proposed by Raphael Cauderlier *)
  | Const (l,cst) as t ->
    let m,v = md cst, id cst in
    if List.mem v ctx && mident_eq (get_module ()) m then
      let v' = (mk_ident ((string_of_mident m) ^ "." ^ (string_of_ident v))) in
      mk_Const l (mk_name m v')
    else
      t
  | App (f,a,args) ->
    mk_App (subst ctx f) (subst ctx a) (List.map (subst ctx) args)
  | Lam (l,x,None,f) ->
    let x' = fresh_name ctx x in
    mk_Lam l x' None (subst (x' :: ctx) f)
  | Lam (l,x,Some a,f) ->
    let x' = fresh_name ctx x in
    mk_Lam l x' (Some (subst ctx a)) (subst (x' :: ctx) f)
  | Pi  (l,x,a,b) ->
    let x' = if is_dummy_ident x then x else fresh_name ctx x in
    mk_Pi l x' (subst ctx a) (subst (x' :: ctx) b)

let rec print_term out = function
  | Kind               -> Format.pp_print_string out "Kind"
  | Type _             -> Format.pp_print_string out "Type"
  | DB  (_,x,n)        -> print_db out (x,n)
  | Const (_,cst)      -> print_const out cst
  | App (f,a,args)     ->
      Format.fprintf out "@[<hov2>%a@]" (print_list " " print_term_wp) (f::a::args)
  | Lam (_,x,None,f)   -> Format.fprintf out "@[%a =>@ @[%a@]@]" print_ident x print_term f
  | Lam (_,x,Some a,f) ->
      Format.fprintf out "@[%a:@,%a =>@ @[%a@]@]" print_ident x print_term_wp a print_term f
  | Pi  (_,x,a,b) when ident_eq x dmark  ->
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
  | Pattern (_,cst,[])   -> Format.fprintf out "%a" print_const cst
  | Pattern (_,cst,pats) -> Format.fprintf out "%a %a" print_const cst (print_list " " print_pattern_wp) pats
  | Lambda (_,x,p)       -> Format.fprintf out "@[%a => %a@]" print_ident x print_pattern p
and print_pattern_wp out = function
  | Pattern _ | Lambda _ as p -> Format.fprintf out "(%a)" print_pattern p
  | Var (_,id,i,(_::_ as lst))     -> Format.fprintf out "(%a %a)" print_db_or_underscore (id,i) (print_list " " print_pattern_wp) lst
  | p -> print_pattern out p

let print_decl fmt (_,x,ty) =
  Format.fprintf fmt "@[<hv2>%a : %a@]" print_ident x print_term ty

let rec print_typed_context fmt = function
  | [] -> ()
  | (_,x,ty) :: decls ->
    Format.fprintf fmt "  @[<hv2>%a : %a@]%s@."
      print_ident x print_term ty (match decls with [] -> "" | _ -> ",");
     print_typed_context fmt decls

let print_err_ctxt fmt = function
  | [] -> ()
  | ctx -> Format.fprintf fmt " in context:@.[\n%a@.]" print_typed_context (List.rev ctx)

let print_rule_name fmt rule =
  let aux b cst =
    if b || !print_default_name
    then
      if mident_eq (md cst) (get_name ())
      then Format.fprintf fmt "@[<h>{%a}@] " print_ident (id cst)
      else Format.fprintf fmt "@[<h>{%a}@] " print_name cst
  in
  match rule with
  | Beta         -> ()
  | Delta(cst)   -> aux true cst (* not printed *)
  | Gamma(b,cst) -> aux b    cst

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
               ctx = [] ;
               (* TODO: here infer context from named variable inside left hand side pattern *)
               pat =  pattern_of_rule_infos ri;
               rhs = ri.rhs
             }
  in
  print_untyped_rule out rule

let print_red_cfg fmt cfg =
  let open Reduction in
  Format.fprintf fmt "%a" pp_red_cfg  cfg

let print_entry fmt e =
  let open Format in
  match e with
  | Decl(_,id,Signature.Static,ty) ->
    fprintf fmt "@[<2>%a :@ %a.@]@.@." print_ident id print_term ty
  | Decl(_,id,Signature.Definable,ty) ->
    fprintf fmt "@[<2>def %a :@ %a.@]@.@." print_ident id print_term ty
  | Def(_,id,opaque,ty,te) ->
    let key = if opaque then "thm" else "def" in
    begin
      match ty with
      | None    -> fprintf fmt "@[<hv2>%s %a@ :=@ %a.@]@.@." key
                     print_ident id print_term te
      | Some ty -> fprintf fmt "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." key
                     print_ident id print_term ty print_term te
    end
  | Rules(_,rs)               ->
    fprintf fmt "@[<v0>%a@].@.@." (print_list "" print_untyped_rule) rs
  | Eval(_,cfg,te)          ->
    fprintf fmt "#EVAL%a %a.@." print_red_cfg cfg print_term te
  | Infer(_,cfg,te)         ->
    fprintf fmt "#INFER%a %a.@." print_red_cfg cfg print_term te
  | Check(_,assrt,neg,test) ->
    let cmd = if assrt then "#ASSERT" else "#CHECK" in
    let neg = if neg then "NOT" else "" in
    begin
      match test with
      | Convert(t1,t2) ->
        fprintf fmt "%s%s %a ==@ %a.@." cmd neg print_term t1 print_term t2
      | HasType(te,ty) ->
        fprintf fmt "%s%s %a ::@ %a.@." cmd neg print_term te print_term ty
    end
  | DTree(_,m,v)            ->
    begin
      match m with
      | None   -> fprintf fmt "#GDT %a.@." print_ident v
      | Some m -> fprintf fmt "#GDT %a.%a.@." print_mident m print_ident v
    end
  | Print(_, str)           ->
    fprintf fmt "#PRINT %S.@." str
  | Name(_,_)               ->
    ()
  | Require(_, md) ->
    fprintf fmt "#REQUIRE %a.@." print_mident md

(** The pretty printer for the type [Signature.staticity] *)
let print_staticity fmt s =
  Format.fprintf fmt "%s"
    (if s=Signature.Static then "Static" else "Definable")
