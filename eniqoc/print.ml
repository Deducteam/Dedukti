open Basic
open Ast

module type Printer =
sig
  val print_name : Format.formatter -> Basic.name -> unit

  val print_ident : Format.formatter -> Basic.ident -> unit

  val print_mident : Format.formatter -> Basic.mident -> unit

  val print_term : Format.formatter -> term -> unit

  val print_declaration : Format.formatter -> declaration -> unit

  val print_definition : Format.formatter -> definition -> unit

  val print_prelude : Format.formatter -> unit -> unit
end

let replace str =
  String.map (fun x -> x) str

let rename kw id =
  if List.mem id kw then
    mk_ident @@ (string_of_ident id) ^ "_"
  else
    let id = string_of_ident id in
    if String.length id > 0 && String.sub id 0 1 = "_" then
      mk_ident @@ "Joker"^(String.sub id 1 (String.length id - 1))
  else
    mk_ident (replace id)


let rec rename_keyword kw term =
  match term with
  | Lam(id, ty, te) ->
    let id' = rename kw id in
    Lam(id', rename_keyword kw ty, rename_keyword kw te)
  | App(f,a) ->
    App(rename_keyword kw f, rename_keyword kw a)
  | Forall(id, ty, te) ->
    let id' = rename kw id in
    Forall(id', rename_keyword kw ty, rename_keyword kw te)
  | Impl(tel,ter) ->
    Impl(rename_keyword kw tel, rename_keyword kw ter)
  | Var(id) ->
    let id' = rename kw id in
    Var(id')
  | _ -> term

module CoqPrinter =
struct

  let keywords = [mk_ident "return"]

  let print_ident out id =
    Format.fprintf out "%a" Pp.print_ident id

  let print_mident out md = Format.fprintf out "%a" Pp.print_mident md

  let print_name out name =
    print_ident out (id name)

  let rec print_term out term =
    match term with
    | Type -> Format.fprintf out "Type"
    | Prop -> Format.fprintf out "Prop"
    | Lam(id, ty, term) -> Format.fprintf out "fun %a : %a => %a" print_ident id print_term ty print_term term
    | App(f,a) -> Format.fprintf out "%a %a" print_term_wp f print_term_wp a
    | Forall(id,ty,term) -> Format.fprintf out "forall %a : %a, %a" print_ident id print_term ty print_term term
    | Impl(tel,ter) -> Format.fprintf out "%a -> %a" print_term_wp tel print_term ter
    | Var(id) -> Format.fprintf out "%a" print_ident id
    | Const(name) -> Format.fprintf out "%a" print_name name

  and print_term_wp out term =
    match term with
    | Prop -> print_term out term
    | t -> Format.fprintf out "(%a)" print_term t

  let print_term out term = print_term out (rename_keyword keywords term)

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

  let print_prelude out () =
    let print_import out md = Format.fprintf out "Require Import %s.@." md in
    List.iter (print_import out) ["leibniz"]

end

module AgdaPrinter =
struct

  let keywords = []

  let print_ident out id =
    Format.fprintf out "%a" Pp.print_ident id

  let print_mident out md = Format.fprintf out "%a" Pp.print_mident md

  let print_name out name = Format.fprintf out "%a"
      Pp.print_ident (rename keywords (id name))

  let rec print_term out term =
    match term with
    | Type -> Format.fprintf out "Set 1"
    | Prop -> Format.fprintf out "Set"
    | Lam(id, ty, term) ->
      Format.fprintf out "\\(%a : %a) -> %a" print_ident id print_term ty print_term term
    | App(f,a) -> Format.fprintf out "%a %a" print_term_wp f print_term_wp a
    | Forall(id,ty,term) ->
      Format.fprintf out "\\all (%a : %a) -> %a" print_ident id print_term ty print_term term
    | Impl(tel,ter) -> Format.fprintf out "%a -> %a" print_term_wp tel print_term ter
    | Var(id) -> Format.fprintf out "%a" print_ident id
    | Const(name) -> Format.fprintf out "%a" print_name name

  and print_term_wp out term =
    match term with
    | Prop | Var _ ->  print_term out term
    | t -> Format.fprintf out "(%a)" print_term t

  let print_term out term = print_term out (rename_keyword keywords term)

  let print_declaration out decl =
    match decl with
    | Axiom(name, term) ->
      Format.fprintf out "postulate %a : %a" print_name name print_term term
    | Parameter(name, term) ->
      Format.fprintf out "postulate %a : %a" print_name name print_term term


    let print_definition out defn =
    match defn with
    | Theorem(name, ty, term) ->
      Format.fprintf out "%a : %a@.%a = %a" print_name name print_term ty
        print_name name print_term term
    | Constant(name, ty, term) ->
      Format.fprintf out "%a : %a@.%a = %a" print_name name print_term ty
        print_name name print_term term

  let print_prelude out () =
    let print_import out md = Format.fprintf out "open import %s@." md in
    List.iter (print_import out) []
end

module MatitaPrinter =
struct
    let keywords = [mk_ident "return"; mk_ident "and"]

  let print_ident out id =
    Format.fprintf out "%a" Pp.print_ident id

  let print_mident out md = Format.fprintf out "%a" Pp.print_mident md

  let print_name out name =
    print_ident out (id name)

  let rec print_term out term =
    match term with
    | Type -> Format.fprintf out "Type[0]"
    | Prop -> Format.fprintf out "Prop"
    | Lam(id, ty, term) -> Format.fprintf out "\\lambda %a : %a. %a" print_ident id print_term ty print_term term
    | App(f,a) -> Format.fprintf out "%a %a" print_term_wp f print_term_wp a
    | Forall(id,ty,term) -> Format.fprintf out "\\forall %a : %a. %a" print_ident id print_term ty print_term term
    | Impl(tel,ter) -> Format.fprintf out "%a \\to %a" print_term_wp tel print_term ter
    | Var(id) -> Format.fprintf out "%a" print_ident id
    | Const(name) -> Format.fprintf out "%a" print_name name

  and print_term_wp out term =
    match term with
    | Prop -> print_term out term
    | t -> Format.fprintf out "(%a)" print_term t

  let print_term out term = print_term out (rename_keyword keywords term)

  let print_declaration out decl =
    match decl with
    | Axiom(name, term) ->
      Format.fprintf out "axiom %a : %a." print_name name print_term term
    | Parameter(name, term) ->
      Format.fprintf out "axiom %a : %a." print_name name print_term term

  let print_definition out defn =
    match defn with
    | Theorem(name, ty, term) ->
      Format.fprintf out "definition %a : %a := %a." print_name name print_term ty print_term term
    | Constant(name, ty, term) ->
      Format.fprintf out "definition %a : %a := %a." print_name name print_term ty print_term term

  let print_prelude out mds =
    let print_import out md = Format.fprintf out "include \"%s\".@." md in
    List.iter (print_import out) ["basics/pts.ma";"leibniz.ma"]
end

let initial = String.uppercase_ascii "initial"

(*
let print_depends out depends =
  List.iter (fun x -> Format.fprintf out "%a@." print_declaration x) depends
  let print_depend out dep =
    match dep with
    | Declaration(decl) -> print_declaration out decl
    | DefConstant(name,ty,te) -> print_definition out (Constant(name,ty,te))
  in
  Format.fprintf out "Module Type %s.@." initial;
  List.iter (fun x -> Format.printf "%a@." print_depend x) depends;
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
*)

let print_obj (module P:Printer) out name obj =
  match obj with
  | Definition(defn) -> Format.fprintf out "%a@." P.print_definition defn
  | Declaration(decl) -> Format.fprintf out "%a@." P.print_declaration decl
(*
  print_depends out obj.depends;
  print_target out name obj.definition;
  print_fonctor out name obj.definition
*)

let printer : (module Printer) ref = ref ((module CoqPrinter):(module Printer))

let set_printer s : unit =
  if s = "coq" then printer := (module CoqPrinter)
  else if s = "agda" then printer := (module AgdaPrinter)
  else if s = "matita" then printer := (module MatitaPrinter)
  else
    failwith (Printf.sprintf "printer %s is not supported" s)


let print_ast out ast =
  let (module P) = !printer in
  P.print_prelude out ();
  List.iter (print_obj !printer out ast.name) ast.obj
