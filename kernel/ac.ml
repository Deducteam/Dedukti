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
let force_flatten_AC_terms
      (snf:term -> term)
      (are_convertible:term -> term -> bool)
      (name,aci) terms =
  let rec aux acc = function
  | [] -> acc
  | hd :: tl ->
     match get_AC_args name hd with
     | Some (a1,a2) -> aux acc (a1 :: a2 :: tl)
     | None         ->
        let snfhd = snf hd in
        match get_AC_args name snfhd with
        | Some (a1,a2) -> aux acc (a1 :: a2 :: tl)
        | None         -> aux (snfhd :: acc) tl in
  let res = aux [] terms in
  (* If aci is an ACU symbol, remove corresponding neutral element. *)
  match aci with
  | ACU neu -> List.filter (fun x -> not (are_convertible neu x)) res
  | _ -> res


(* Reduces subterms with f to have a maximal set of elements. *)
let force_flatten_AC_term
      (snf:term -> term)
      (are_convertible:term -> term -> bool)
      aci t = force_flatten_AC_terms snf are_convertible aci [t]

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
