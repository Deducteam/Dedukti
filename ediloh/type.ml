(** Type variables *)
type var = string

(** Type operators *)
type op = string

type hol_type =
  | Var of var
  | App of op * hol_type list


let d_bool = Basic.hstring "prop"

let d_hol = Basic.hstring "hol"

let d_arrow = Basic.hstring "arrow"

let d_poly_arrow = Basic.hstring "forall_kind_type"

let h_arrow = "->"


let is_arrow (Term.Const(_,m,id)) = Basic.ident_eq m d_hol && Basic.ident_eq id d_arrow

let is_kind (Term.Const(_,m,id)) = Basic.ident_eq m d_hol && Basic.ident_eq id d_poly_arrow

let prefix m id = (Basic.string_of_ident m)^"__"^(Basic.string_of_ident id)

let rec translate_type t =
  match t with
  | Term.DB(_, x, _) -> Var(Basic.string_of_ident x)
  | Term.Const(_, m, id) -> Var(prefix m id)
  | Term.App(op, arg, [arg']) when is_arrow op -> App(h_arrow, [(translate_type arg);(translate_type arg')])
  | _ -> failwith "is not a valid hol type"

let rec translate_kind t =
  match t with
  | Term.App(op, Term.Lam(_,x,pty,te), []) when is_kind op -> App(h_arrow, [Var(Basic.string_of_ident x); translate_kind te])
  | _ -> translate_type t


let test = ignore (translate_kind (Term.mk_App (Term.mk_Const Basic.dloc d_hol d_poly_arrow) (Term.mk_Lam Basic.dloc (Basic.hstring "A") None (Term.mk_App (Term.mk_Const Basic.dloc d_hol d_arrow) (Term.mk_DB Basic.dloc (Basic.hstring "A") 0) [(Term.mk_App (Term.mk_Const Basic.dloc d_hol d_arrow) (Term.mk_DB Basic.dloc (Basic.hstring "A") 0) [Term.mk_Const Basic.dloc d_hol d_bool])])) []))
