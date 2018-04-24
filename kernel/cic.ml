open Basic
open Term

type univ =
  | Prop
  | Type of int

let cic = mk_mident "cic"

let mk_const id = mk_Const dloc (mk_name cic id)

let z = mk_name cic (mk_ident "z")

let s = mk_name cic (mk_ident "s")

let succ = mk_name cic (mk_ident "succ")

let sort = mk_name cic (mk_ident "Sort")

let cast = mk_name cic (mk_ident "cast")

let max = mk_name cic (mk_ident "max")

let rule = mk_name cic (mk_ident "rule")

let prop = mk_name cic (mk_ident "prop")

let typ = mk_name cic (mk_ident "type")

let univ = mk_name cic (mk_ident "Univ")

let cuni = mk_name cic (mk_ident "univ")

let term = mk_name cic (mk_ident "Term")

let prod = mk_name cic (mk_ident "prod")

let is_const cst t =
  match t with
  | Const(_,n) -> name_eq cst n
  | _ -> false

let is_z t =
  match t with
  | Const(_,u) when is_const z t -> true
  | _ -> false

let is_s t =
  match t with
  | App(u,_,[]) when is_const s u -> true
  | _ -> false

let is_term t =
  match t with
  | App(u,_,[_]) when is_const term u -> true
  | _ -> false

let is_univ t =
  match t with
  | App(u,_,[]) when is_const univ u -> true
  | _ -> false

let is_cuni t =
  match t with
  | App(u,_,[]) when is_const cuni u -> true
  | _ -> false

let is_prop t =
  match t with
  | Const(_,n) when is_const prop t -> true
  | _ -> false

let is_type t =
  match t with
  | App(t,_,[]) when is_const typ t -> true
  | _ -> false

let is_succ t =
  match t with
  | App(c,arg,[]) when is_const succ c -> true
  | _ -> false

let is_cast t =
  match t with
  | App(c, s1, [s2;t1;t2;a]) when is_const cast c -> true
  | _ -> false

let is_max t =
  match t with
  | App(c, s1, [s2]) when is_const max c -> true
  | _ -> false

let is_rule t =
  match t with
  | App(c, s1, [s2]) when is_const rule c -> true
  | _ -> false

let is_prod t =
  match t with
  | App(c, s1, [s2;a;f]) when is_const prod c -> true
  | _ -> false

let is_var t =
  match t with
  | DB _ -> true
  | _ -> false

let is_lam t =
  match t with
  | Lam _ -> true
  | _ -> false

let is_app t =
  match t with
  | App _ -> true
  | _ -> false

let extract_app t =
  match t with
  | App (f,a,args) -> f,a,args
  | _ -> failwith "is not an app"

let extract_var t =
  match t with
  | DB(_,id,_) -> id
  | _ -> failwith "is not a local variable"

let extract_s t =
  match t with
  | App(t,u,[]) when is_const s t -> u
  | _ -> failwith "is not a s"

let extract_type t =
  match t with
  | App(t,u,[]) when is_const typ t -> u
  | _ -> failwith "is not a type"

let extract_term t =
  match t with
  | App(t,s,[u]) when is_const term t -> s,u
  | _ -> failwith "is not a term"

let extract_succ t =
  match t with
  | App(c,arg,[]) when is_const succ c -> arg
  | _ -> failwith "is not a succ"

let extract_cast t =
  match t with
  | App(c,s1,[s2;t1;t2;a]) when is_const cast c -> s1,s2,t1,t2,a
  | _ -> failwith "is not a cast"

let extract_max t =
  match t with
  | App(c,s1,[s2]) when is_const max c -> s1,s2
  | _ -> failwith "is not a max"

let extract_rule t =
  match t with
  | App(c, s1, [s2]) when is_const rule c -> s1, s2
  | _ -> failwith "is not a rule"

let extract_univ t =
  match t with
  | App(c, s, []) when is_const univ c -> s
  | _ -> failwith "is not a univ"

let extract_cuni t =
  match t with
  | App(c, s, []) when is_const cuni c -> s
  | _ -> failwith "is not a cuni"

let extract_prod t =
  match t with
  | App(c, s1, [s2;a;f]) when is_const prod c -> s1,s2,a,f
  | _ -> failwith "is not a prod"

let extract_lam t =
  match t with
  | Lam(_,x,Some ty,te) -> x,ty,te
  | _ -> failwith "not a lambda or lambda without a type"

let extract_succ t =
  match t with
  | App(c,arg,[]) when is_const succ c -> arg
  | _ -> failwith "is not a succ"

let mk_prop     = mk_Const dloc prop

let mk_z        = mk_Const dloc z

let mk_s arg    = mk_App (mk_Const dloc s) arg []

let mk_type arg = mk_App (mk_Const dloc typ) arg []

let mk_succ arg = mk_App (mk_Const dloc succ) arg []

let mk_rule left right  = mk_App (mk_Const dloc rule) left [right]

let mk_max  left right  = mk_App (mk_Const dloc max) left [right]

let mk_univ s =
  mk_App (mk_Const dloc univ) s []

let mk_cuni s =
  mk_App (mk_Const dloc cuni) s []

let mk_cast s1 s2 t1 t2 a =
  mk_App (mk_Const dloc cast) s1 [s2;t1;t2;a]

let mk_prod s1 s2 a x ty te =
  mk_App (mk_Const dloc prod) s1 [s2;a;(mk_Lam dloc x (Some ty) te)]

let mk_term s a =
  mk_App (mk_Const dloc term) s [a]

let assert_type_zero t =
  if is_z (extract_type t) then
    ()
  else
    failwith "This bug should be reported (assert_type_zero)"
