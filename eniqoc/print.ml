open Basic
open Ast

let rename id = hstring @@ (string_of_ident id) ^ "_"

let keyword = [hstring "return"]

let rec rename_keyword term =
  match term with
  | Lam(id, ty, te) ->
    let id' = if List.mem id keyword then rename id else id in
    Lam(id', rename_keyword ty, rename_keyword te)
  | App(f,a) ->
    App(rename_keyword f, rename_keyword a)
  | Forall(id, ty, te) ->
    let id' = if List.mem id keyword then rename id else id in
    Forall(id', rename_keyword ty, rename_keyword te)
  | Impl(tel,ter) ->
    Impl(rename_keyword tel, rename_keyword ter)
  | Var(id) ->
    let id' = if List.mem id keyword then rename id else id in
    Var(id')
  | _ -> term

let print_ident out id =
  Format.fprintf out "%a" Pp.print_ident id

let print_name out (md,id) =
  print_ident out id

let rec print_term out term =
  match term with
  | Type -> Format.fprintf out "Type"
  | Prop -> Format.fprintf out "Prop"
  | Lam(id, ty, term) -> Format.fprintf out "fun %a : %a => %a" print_ident id print_term ty print_term term
  | App(f,a) -> Format.fprintf out "%a %a" print_term_wp f print_term_wp a
  | Forall(id,ty,term) -> Format.fprintf out "forall %a : %a, %a" print_ident id print_term ty print_term term
  | Impl(tel,ter) -> Format.fprintf out "%a -> %a" print_term_wp tel print_term ter
  | Var(id) -> Format.fprintf out "%a" print_ident id
  | Const((md,id)) -> Format.fprintf out "%a" print_name (md,id)

and print_term_wp out term =
  match term with
  | Prop -> print_term out term
  | t -> Format.fprintf out "(%a)" print_term t

let print_term out term = print_term out (rename_keyword term)

let print_declaration out decl =
  match decl with
  | Axiom(name, term) ->
    Format.fprintf out "Axiom %a : %a." print_name name print_term term
  | Parameter(name, term) ->
    Format.fprintf out "Parameter %a : %a." print_name name print_term term

let print_definition out defn =
  match defn with
  | Theorem(name, ty, term) ->
    Format.fprintf out "Definition %a : %a := %a." print_name name print_term ty print_term term
  | Constant(name, ty, term) ->
    Format.fprintf out "Definition %a : %a := %a." print_name name print_term ty print_term term

let initial = String.uppercase_ascii "initial"

let print_depends out depends =
  Format.fprintf out "Module Type %s.@." initial;
  List.iter (fun x -> print_declaration out x;Format.printf "@.") depends;
  Format.fprintf out "End %s.@." initial

let forget_definition defn =
  match defn with
  | Theorem(n,ty,_) -> Axiom(n,ty)
  | Constant(n,ty,_) -> Parameter(n,ty)

let print_target out name defn =
  let name = String.uppercase_ascii name in
  Format.fprintf out "Module Type %s.@." name;
  Format.fprintf out "Declare Module M : %s.@." initial;
  Format.fprintf out "Import M.@.";
  List.iter (fun x -> print_declaration out x;Format.printf "@.") (List.map forget_definition defn);
  Format.fprintf out "End %s.@." name

let print_fonctor out name defn =
  let name' = String.uppercase_ascii name in
  Format.fprintf out "Module %s (M : %s) : %s.@." name initial name';
  Format.fprintf out "Module M := M.@.";
  Format.fprintf out "Import M.@.";
  List.iter (fun x -> print_definition out x; Format.printf "@.") defn;
  Format.fprintf out "End %s." name

let print_obj out name obj =
  print_depends out obj.depends;
  print_target out name obj.definition;
  print_fonctor out name obj.definition

let print_module_id out md = Format.fprintf out "%s" md

let print_prelude out mds =
  let print_import out md = Format.fprintf out "Require Import %s.@." md in
  List.iter (print_import out) mds

let print_ast out ast =
  print_prelude out ast.prelude;
  print_obj out ast.name ast.obj
