open Basic
open Term

(*   AC identifier (name and algebra).   *)

type ac_ident = name * algebra

let ac_ident_eq (name,_) (name',_) = name_eq name name'

let pp_ac_ident fmt (name,_) = Format.fprintf fmt "%a" pp_name name

(* AC functions *)

let get_AC_args name = function
  | App( Const (_,name'), a1, [a2]) when name_eq name name' -> Some (a1, a2)
  | _ -> None

(* Reduces subterms with f to have a maximal set of elements. *)
let force_flatten_AC_terms f name =
  let rec aux acc = function
  | [] -> acc
  | hd :: tl ->
     match get_AC_args name hd with
     | Some (a1,a2) -> aux acc (a1 :: a2 :: tl)
     | None         ->
        let fhd = f hd in
        match get_AC_args name fhd with
        | Some (a1,a2) -> aux acc (a1 :: a2 :: tl)
        | None         -> aux (fhd :: acc) tl in
  aux []

(* Reduces subterms with f to have a maximal set of elements. *)
let force_flatten_AC_term f name t = force_flatten_AC_terms f name [t]

let flatten_AC_terms name =
  let rec aux acc = function
  | [] -> acc
  | hd :: tl ->
     match get_AC_args name hd with
     | Some (a1,a2) -> aux acc (a1 :: a2 :: tl)
     | None         -> aux (hd :: acc) tl in
  aux []

let flatten_AC_term name t = flatten_AC_terms name [t]

let rec unflatten_AC (name,alg) = function
  | [] -> ( match alg with ACU neu -> neu | _ -> assert false )
  | [t] -> t
  | t1 :: t2 :: tl ->
     unflatten_AC (name,alg) ((mk_App (mk_Const dloc name) t1 [t2]) :: tl)
