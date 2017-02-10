open Basic
open Opentheory

let print_term t = Pp.print_term Format.std_formatter t;Format.printf "@."

(* ********************************* *)

let counter = ref 0

let hol_module = hstring "hol"
let hol_sort = hstring "type"
let hol_eta = hstring "eta"
let hol_arrow = hstring "arrow"
let hol_forall = hstring "forall"
let hol_impl = hstring "impl"
let hol_prop = hstring "prop"
let hol_eps = hstring "eps"
let hol_forall_kind_type = hstring "forall_kind_type"
let hol_forall_kind_prop = hstring "forall_kind_prop"

let (===) = Basic.ident_eq

let is_hol_const c t =
  match t with
  | Term.Const(_, m, id) -> (m === hol_module) &&  (c === id)
  | _ -> false

let const_env:(Basic.ident * ty) list ref = ref []

let is_hol_sort t = is_hol_const hol_sort t

let is_hol_eta t = is_hol_const hol_eta t

let is_hol_arrow t = is_hol_const hol_arrow t

let is_hol_forall t = is_hol_const hol_forall t

let is_hol_impl t = is_hol_const hol_impl t

let is_hol_prop t = is_hol_const hol_prop t

let is_hol_eps t = is_hol_const hol_eps t

let is_hol_forall_kind_type t = is_hol_const hol_forall_kind_type t

let is_hol_forall_kind_prop t = is_hol_const hol_forall_kind_prop t

let is_hol_type t =
  match t with
  | Term.App (c, _, _) -> is_hol_eta c
  | _ -> false

let is_hol_proof t =
  match t with
  | Term.App (c, _, _) -> is_hol_eps c
  | _ -> false

let extract_type t =
  match t with
  | Term.App(c, ty, _) when is_hol_eta c -> ty
  | _ -> assert false

let extract_proof t =
  match t with
  | Term.App(c, ty, _) when is_hol_eps c -> ty
  | _ -> assert false


let extract_term t =
  match t with
  | Term.App(c, ty, _) when is_hol_eps c -> ty
  | _ -> assert false

let rec instr_of_type t =
  match t with
  | Term.Const(_,m,id) when is_hol_prop t ->
    mk_bool_type
  | Term.Const(_,m, id) ->
    (* ASSUMPTION : every type operator is of arity 0 *)
    ty_of_tyop (mk_tyop (mk_name [] (string_of_ident id))) []
  (* ASSUMPTION : no clash in names and id should start with a majuscule *)
  | Term.DB(_,id,i) ->
    mk_varType (mk_name [] (string_of_ident id))
  | Term.App(c, tyl, [tyr]) when is_hol_arrow c ->
    mk_arrow_type (instr_of_type tyl) (instr_of_type tyr)
  | Term.App(c, Term.Lam(_,x, Some tx, ty), []) when is_hol_forall_kind_type c ->
    instr_of_type ty
  | _ -> Pp.print_term Format.std_formatter t; failwith "todo type"

type env = (Basic.ident * Term.term) list

let is_type_variable env a =
  match a with
  | Term.DB(_,var,_) -> List.mem_assoc var env && is_hol_sort (List.assoc var env)
  | _ -> false

(* TODO add a dictionnary *)
let rec instr_of_term env t =
  match t with
  | Term.App(c, ty, [te])  when is_hol_forall c ->
    mk_forall_term (instr_of_term env te) (instr_of_type ty)
  | Term.App(c, tel, [ter]) when is_hol_impl c ->
    mk_impl_term (instr_of_term env tel) (instr_of_term env ter)
  | Term.App(c, Term.Lam(_,x, Some tx, ty), []) when is_hol_forall_kind_prop c ->
    instr_of_term ((x,tx)::env) ty
  | Term.App(f, a, args) ->
    List.fold_left
      (fun instr a ->
         if is_type_variable env a then
             instr
           else
             mk_appTerm instr (instr_of_term env a)
      ) (instr_of_term env f) (a::args)
  | Term.Lam(_,x,Some tx, t') when is_hol_sort tx ->
    instr_of_term ((x,tx)::env) t'
  | Term.Lam(_,x,Some tx, t') ->
    let tx' = extract_type tx in
    let it = (instr_of_term ((x,tx')::env) t') in
    let ty = instr_of_type tx' in
    mk_absTerm (mk_var (mk_name [] (string_of_ident x)) ty) it
  | Term.Lam(_, _, None, _) -> failwith "every lambda should be typed"
  | Term.DB(_,id,i) ->
    (* ASSUMPTION : no clash in names variable as in \x\x.x*)
    let itx = instr_of_type (List.assoc id env) in
    mk_varTerm (mk_var (mk_name [] (string_of_ident id)) itx)
  | Term.Const(_,_,id) ->
    let ty = List.assoc id !const_env in
    term_of_const (const_of_name (mk_name [] (string_of_ident id))) ty
  | _ -> Pp.print_term Format.std_formatter t; failwith "todo term"


let rec instr_of_proof ctx t =
  match t with
  | Term.Lam(_,x, Some ty, t') when is_hol_sort ty ->
    instr_of_proof ctx t'
  | Term.Lam(_,x, Some ty, t') when is_hol_type ty ->
    let ty' = (instr_of_type (extract_type ty)) in
    let t,_,thm = instr_of_proof ((x,(extract_type ty))::ctx) t' in
    mk_rule_intro_forall (mk_name [] (string_of_ident x)) ty' t thm
  | Term.Lam(_,x, Some ty, t') when is_hol_proof ty ->
    let p = (instr_of_term ctx (extract_proof ty)) in
    let q,_,thm = instr_of_proof ((x,(extract_proof ty))::ctx) t' in
    mk_rule_intro_impl thm p q
  | Term.DB(_,id,_) ->
    let ty' = (instr_of_term ctx (List.assoc id ctx)) in
    ty', (mk_hyp [ty']), mk_assume ty'
  | _ -> (Pp.print_term Format.std_formatter t; failwith "instr_of_proof")

let mk_prelude lc name = ()

(* Assume that one defines an external type operator id *)
let define_hol_type id = ()


let define_hol_const id te = mk_const (mk_name [] (string_of_ident id)) (instr_of_term [] te)

let define_axiom term =
  let term' = instr_of_term [] term in
  mk_thm term' (mk_hyp []) (mk_axiom (mk_hyp []) term')

let define_thm term hyp thm =
  mk_thm term hyp thm

let mk_declaration lc id pty : unit =
  if is_hol_sort pty then
    define_hol_type id
  else
    if is_hol_type pty then
      failwith "should not happen : reference to an external constant"
    else
      if is_hol_proof pty then
        let term = extract_term pty in
        define_axiom term
      else
        failwith "case not handle"

let mk_definable lc id pty : unit = failwith "definable symbol without definition... should not happend"

let mk_definition lc id pty_opt pte : unit =
  let ty =
    match pty_opt with
    | Some ty -> ty
    | None -> failwith "missing type in the definition"
  in
  if is_hol_type ty then
    begin
      const_env := (id, instr_of_type (extract_type ty))::!const_env;
      define_hol_const id pte
    end
  else
  if is_hol_proof ty then
    let _,_,thm = instr_of_proof [] pte in
    mk_thm (instr_of_term [] (extract_term ty)) (mk_hyp []) thm
  else
    failwith "case not handle"


let mk_opaque lc id pty_opt pte =
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Rule.Pattern (l,md,id,_) -> (l,md,id)
  | _ -> (dloc,qmark,qmark)

let mk_rules = function
  | [] -> ()
  | ((_,pat,_)::_) as lst ->
    begin
      match Env.add_rules lst with
      | OK _ -> ()
      | Err e -> Errors.fail_env_error e
    end

let mk_command = Cmd.mk_command

let export = ref false

let mk_ending () =
  Printf.printf "%s\n" (Opentheory.to_string ())
