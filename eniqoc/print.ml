open Basic
open Ast

let post id = "_"

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
      (*
  | Eq(ty,(left,right)) ->
    let ty' = rename_keyword ty in
    let left' = rename_keyword left in
    let right' = rename_keyword right in
    Eq(ty', (left', right')) *)
  | _ -> term

let print_prelude out () =
  Format.fprintf out "Definition leibniz (A:Type) (x y:A) := forall P, P x -> P y.@.";
  Format.fprintf out "Axiom sym_leibniz : forall A:Type, forall (x y:A), leibniz A x y -> leibniz A y x.@."

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

let print_obj out obj =
  match obj with
  | Axiom(name, term) ->
    Format.fprintf out "Axiom %a : %a." print_name name print_term term
  | Parameter(name, term) ->
    Format.fprintf out "Parameter %a : %a." print_name name print_term term
  | Constant(name, ty, term) ->
    Format.fprintf out "Definition %a : %a := %a." print_name name print_term ty print_term term
  | Theorem(name, ty, term) ->
    Format.fprintf out "Definition %a : %a := %a." print_name name print_term ty print_term term
