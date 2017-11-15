open Basic
open Term

type ac_ident = ident * ident * algebra

let ac_ident_eq (m,v,ac) (m',v',ac') = assert(ac == ac'); ident_eq m m' && ident_eq v v'

let pp_ac_ident fmt (m,v,ac) =
  Format.fprintf fmt "%a.%a" pp_ident m pp_ident v

let is_acu (_,_,alg) = match alg with | ACU _ -> true | _ -> false

let get_AC_args m v = function
  | App( Const (l,m',v'), a1, [a2])
       when ident_eq m m' && ident_eq v v' ->
     Some (a1, a2)
  | t -> None

(* Reduces subterms with f to have a maximal set of elements. *)
let force_flatten_AC_terms f m v =
  let rec aux acc = function
  | [] -> acc
  | hd :: tl ->
     match get_AC_args m v hd with
     | Some (a1,a2) -> aux acc (a1 :: a2 :: tl)
     | None         ->
        let fhd = f hd in
        match get_AC_args m v fhd with
        | Some (a1,a2) -> aux acc (a1 :: a2 :: tl)
        | None         -> aux (fhd :: acc) tl in
  aux []

(* Reduces subterms with f to have a maximal set of elements. *)
let force_flatten_AC_term f m v t = force_flatten_AC_terms f m v [t]

let flatten_AC_terms m v =
  let rec aux acc = function
  | [] -> acc
  | hd :: tl ->
     match get_AC_args m v hd with
     | Some (a1,a2) -> aux acc (a1 :: a2 :: tl)
     | None         -> aux (hd :: acc) tl in
  aux []

let flatten_AC_term m v t = flatten_AC_terms m v [t]

let rec unflatten_AC (m,v,alg) = function
  | [] -> ( match alg with ACU neu -> neu | _ -> assert false )
  | [t] -> t
  | t1 :: t2 :: tl ->
     unflatten_AC (m,v,alg) ((mk_App (mk_Const dloc m v) t1 [t2]) :: tl)
