open Basic

type uvar = ident

let basename = "?"

exception Not_uvar

let is_uvar t =
  match t with
  | Term.Const(_,n) ->
    let s = string_of_ident (id n) in
    let n = String.length basename in
    String.length s > n && String.sub s 0 n = basename
  | _ -> false

let ident_of_uvar t =
  match t with
  | Term.Const(_,n) when is_uvar t -> id n
  | _ -> Format.printf "%a@." Term.pp_term t; raise Not_uvar

let counter = ref 0

let fresh () =
  let name = Format.sprintf "%s%d" basename !counter in
  incr counter; mk_ident name

let fresh_uvar sg =
  let id = fresh () in
  let md = Signature.get_name sg in
  let name = Basic.mk_name md id in
  let cst = Term.mk_Const Basic.dloc name in
  Signature.add_declaration sg Basic.dloc id Signature.Static
    (Term.mk_Const Basic.dloc (Basic.mk_name (Basic.mk_mident "cic") (Basic.mk_ident "Sort")));
  cst
