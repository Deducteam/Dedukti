open Basic
open Term
open Rule
open Entry
open Format

(* FIXME: this module is highly redondant with printing functions insides kernel modules *)

(* TODO: make that debuging functions returns a string *)
let print_db_enabled = ref false
let print_default_name = ref false

let print_list = pp_list

let print_ident = pp_ident

let print_mident = pp_mident

let print_name = pp_name

(** The pretty printer for the type [Signature.staticity] *)
let print_staticity fmt s =
  fprintf fmt "%s"
    (if s=Signature.Static then "Static" else "Definable")

let print_red_cfg fmt cfg =
  let open Reduction in
  fprintf fmt "%a" pp_red_cfg  cfg


module type Sig =
sig
  type t
  val get_name : t -> mident
  (** [get_name] get the current module defined for printing functions. *)
end

module type Printer =
sig
  type t
  val print_term          : t -> term                printer
  val print_typed_context : t -> typed_context       printer
  val print_err_ctxt      : t -> typed_context       printer
  val print_pattern       : t -> Rule.pattern        printer
  val print_untyped_rule  : t -> Rule.untyped_rule   printer
  val print_typed_rule    : t -> Rule.typed_rule     printer
  val print_rule_infos    : t -> Rule.rule_infos     printer
  val print_rule_name     : t -> Rule.rule_name      printer
  val print_entry         : t -> Entry.entry         printer
end

module Make (S:Sig) : (Printer with type t = S.t) =
struct
type t = S.t
let print_const sg out cst =
  let md = md cst in
  if mident_eq md (S.get_name sg) then print_ident out (id cst)
  else fprintf out "%a" pp_name cst

(* Idents generated from underscores by the parser start with a question mark.
   We have sometimes to avoid to print them because they are not valid tokens. *)
let is_dummy_ident   i = (string_of_ident i).[0] =  '$'
let is_regular_ident i = (string_of_ident i).[0] <> '$'

let print_db out (x,n) =
  if !print_db_enabled then fprintf out "%a[%i]" print_ident x n
  else print_ident out x

let print_db_or_underscore out (x,n) =
  if is_dummy_ident x then fprintf out "_"
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

let rec subst sg ctx = function
  | DB (_,x,_) as t when is_dummy_ident x -> t
  | DB (l,x,n) as t -> ( try mk_DB l (List.nth ctx n) n with Failure _ -> t)
  | Kind
  | Type _ as t -> t
  (* if there is a local variable that have the same name as a top level constant,
        then the module has to be printed *)
  (* a hack proposed by Raphael Cauderlier *)
  | Const (l,cst) as t ->
    let m,v = md cst, id cst in
    if List.mem v ctx && mident_eq (S.get_name sg) m then
      let v' = (mk_ident ((string_of_mident m) ^ "." ^ (string_of_ident v))) in
      mk_Const l (mk_name m v')
    else
      t
  | App (f,a,args) ->
    mk_App (subst sg ctx f) (subst sg ctx a) (List.map (subst sg ctx) args)
  | Lam (l,x,None,f) ->
    let x' = fresh_name ctx x in
    mk_Lam l x' None (subst sg (x' :: ctx) f)
  | Lam (l,x,Some a,f) ->
    let x' = fresh_name ctx x in
    mk_Lam l x' (Some (subst sg ctx a)) (subst sg (x' :: ctx) f)
  | Pi  (l,x,a,b) ->
    let x' = if is_dummy_ident x then x else fresh_name ctx x in
    mk_Pi l x' (subst sg ctx a) (subst sg (x' :: ctx) b)

(* This overrides Term.pp_term with above aoptimizations *)
let rec pp_term sg fmt te =
  match te with
  | Kind               -> fprintf fmt "Kind"
  | Type _             -> fprintf fmt "Type"
  | DB  (_,x,n)        -> fprintf fmt "%a" print_db (x,n)
  | Const (_,n)        -> fprintf fmt "%a" (print_const sg) n
  | App (f,a,args)     -> pp_list " " (pp_term_wp sg) fmt (f::a::args)
  | Lam (_,x,None,f)   -> fprintf fmt "%a => %a" pp_ident x (pp_term sg) f
  | Lam (_,x,Some a,f) -> fprintf fmt "%a:%a => %a" pp_ident x (pp_term_wp sg) a (pp_term sg) f
  | Pi  (_,x,a,b)      ->
    if ident_eq x dmark
    then fprintf fmt "%a -> %a" (pp_term_wp sg) a (pp_term sg) b
    else fprintf fmt "%a:%a -> %a" pp_ident x (pp_term_wp sg) a (pp_term sg) b

and pp_term_wp sg fmt te =
  match te with
  | Kind | Type _ | DB _ | Const _ as t -> (pp_term sg) fmt t
  | t                                   -> fprintf fmt "(%a)" (pp_term sg) t

let rec print_term sg m out t =
  let one_liner = asprintf "%a" (pp_term sg) t in
  if String.length one_liner <= m then fprintf out "%s" one_liner
  else
    match t with
    | Kind               -> pp_print_string out "Kind"
    | Type _             -> pp_print_string out "Type"
    | DB  (_,x,n)        -> print_db out (x,n)
    | Const (_,cst)      -> print_const sg out cst
    | App (f,a,args)     ->
      fprintf out "@[<v 2>%a@]"
        (pp_print_list (print_term_wp sg (m-2))) (f::a::args)
    | Lam (_,x,None,f)   ->
      fprintf out "@[%a =>@ @[%a@]@]"
        print_ident x (print_term sg m) f
    | Lam (_,x,Some a,f) ->
      fprintf out "@[<v>%a:%a =>@ @[%a@]@]"
        print_ident x (print_term_wp sg (m - (String.length (string_of_ident x)) - 3)) a
        (print_term sg m) f
    | Pi  (_,x,a,b) when ident_eq x dmark  ->
      (* arrow, no pi *)
      fprintf out "@[<v>%a ->@ @[%a@]@]"
        (print_term_wp sg (m-3)) a (print_term sg m) b
    | Pi  (_,x,a,b)      ->
      fprintf out "@[<v>%a:%a ->@ @[%a@]@]"
        print_ident x (print_term_wp sg m) a (print_term sg m) b

and print_term_wp sg m out = function
  | Kind | Type _ | DB _ | Const _ as t -> print_term sg m out t
  | t                                   -> fprintf out "(%a)" (print_term sg (m-2)) t

(* Overwrite print_term by a name-clash avoiding version *)
let n_print_term sg n out t = print_term sg n out (subst sg [] t)

let line_length = 100

(* Printing on default line length *)
let print_term sg out t = n_print_term sg line_length out (subst sg [] t)

let print_bv out (_,id,i) = print_db out (id,i)

let rec print_pattern sg out = function
  | Var (_,id,i,[]) -> print_db_or_underscore out (id,i)
  | Var (_,id,i,lst)     -> fprintf out "%a %a" print_db_or_underscore (id,i) (print_list " " (print_pattern_wp sg)) lst
  | Brackets t           -> fprintf out "{ %a }" (print_term sg) t
  | Pattern (_,cst,[])   -> fprintf out "%a" (print_const sg) cst
  | Pattern (_,cst,pats) -> fprintf out "%a %a" (print_const sg) cst (print_list " " (print_pattern_wp sg)) pats
  | Lambda (_,x,p)       -> fprintf out "@[%a => %a@]" print_ident x (print_pattern sg) p
and print_pattern_wp sg out = function
  | Pattern _ | Lambda _ as p -> fprintf out "(%a)" (print_pattern sg) p
  | Var (_,id,i,(_::_ as lst))     -> fprintf out "(%a %a)" print_db_or_underscore (id,i) (print_list " " (print_pattern_wp sg)) lst
  | p -> print_pattern sg out p

let print_decl sg fmt (_,x,ty) =
  fprintf fmt "@[<v>%a : %a@]"
    print_ident x (n_print_term sg (line_length - 5 - String.length (string_of_ident x))) ty

let rec print_typed_context sg fmt = function
  | [] -> ()
  | (_,x,ty) :: decls ->
    fprintf fmt "  @[<hv2>%a : %a@]"
      print_ident x (print_term sg) ty;
    (match decls with | [] -> () | _ -> fprintf fmt ",@.");
     print_typed_context sg fmt decls

let print_err_ctxt sg fmt = function
  | [] -> ()
  | ctx -> fprintf fmt " in context:@.[\n%a@.]" (print_typed_context sg) (List.rev ctx)

let print_rule_name sg fmt rule =
  let aux b cst =
    if b || !print_default_name
    then
      if mident_eq (md cst) (S.get_name sg)
      then fprintf fmt "@[<h>{%a}@] " print_ident (id cst)
      else fprintf fmt "@[<h>{%a}@] " print_name cst
  in
  match rule with
  | Beta         -> ()
  | Delta(cst)   -> aux true cst (* not printed *)
  | Gamma(b,cst) -> aux b    cst

let print_untyped_rule sg fmt (rule:untyped_rule) =
  let print_decl out (_,id) =
    fprintf out "@[<hv>%a@]" print_ident id
  in
  fprintf fmt
    "@[<hov2>%a@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_rule_name sg) rule.name
    (print_list ", " print_decl) (List.filter (fun (_, id) -> is_regular_ident id) rule.ctx)
    (print_pattern sg) rule.pat
    (print_term sg) rule.rhs

let print_typed_rule sg out (rule:typed_rule) =
  let print_decl out (_,id,ty) =
    fprintf out "@[<hv>%a:@,%a@]" print_ident id (print_term sg) ty
  in
  fprintf out
    "@[<hov2>@[<h>[%a]@]@ @[<hv>@[<hov2>%a@]@ -->@ @[<hov2>%a@]@]@]@]"
    (print_list ", " print_decl) rule.ctx
    (print_pattern sg) rule.pat
    (print_term sg) rule.rhs

let print_rule_infos sg out ri =
  let rule = { name = ri.name ;
               ctx = [] ;
               (* TODO: here infer context from named variable inside left hand side pattern *)
               pat =  pattern_of_rule_infos ri;
               rhs = ri.rhs
             }
  in
  print_untyped_rule sg out rule

let print_entry sg fmt e =
  let open Format in
  match e with
  | Decl(_,id,Signature.Static,ty) ->
    fprintf fmt "@[<2>%a :@ %a.@]@.@." print_ident id (print_term sg) ty
  | Decl(_,id,Signature.Definable,ty) ->
    fprintf fmt "@[<2>def %a :@ %a.@]@.@." print_ident id (print_term sg) ty
  | Def(_,id,opaque,ty,te) ->
    let key = if opaque then "thm" else "def" in
    begin
      match ty with
      | None    -> fprintf fmt "@[<hv2>%s %a@ :=@ %a.@]@.@." key
                     print_ident id (print_term sg) te
      | Some ty -> fprintf fmt "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." key
                     print_ident id (print_term sg) ty (print_term sg) te
    end
  | Rules(_,rs)               ->
    fprintf fmt "@[<v0>%a@].@.@." (print_list "" (print_untyped_rule sg)) rs
  | Eval(_,cfg,te)          ->
    fprintf fmt "#EVAL%a %a.@." print_red_cfg cfg (print_term sg) te
  | Infer(_,cfg,te)         ->
    fprintf fmt "#INFER%a %a.@." print_red_cfg cfg (print_term sg) te
  | Check(_,assrt,neg,test) ->
    let cmd = if assrt then "#ASSERT" else "#CHECK" in
    let neg = if neg then "NOT" else "" in
    begin
      match test with
      | Convert(t1,t2) ->
        fprintf fmt "%s%s %a ==@ %a.@." cmd neg (print_term sg) t1 (print_term sg) t2
      | HasType(te,ty) ->
        fprintf fmt "%s%s %a ::@ %a.@." cmd neg (print_term sg) te (print_term sg) ty
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
end

module Default = Make(
  struct
    type t = unit
    let get_name () = Basic.mk_mident ""
  end)

module WithModname = Make(
  struct
    type t = mident
    let get_name i = i
  end)
