open Basic
open Format

(** {2 Terms/Patterns} *)

type term =
  | Kind                                             (* Kind *)
  | Type  of loc                                     (* Type *)
  | DB    of loc * ident * int                       (* deBruijn *)
  | Const of loc * name                              (* Global variable *)
  | App   of term * term * term list                 (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc * ident * term option * term        (* Lambda abstraction *)
  | Pi    of loc * ident * term * term               (* Pi abstraction *)

let rec pp_term fmt te =
  match te with
  | Kind               -> fprintf fmt "Kind"
  | Type _             -> fprintf fmt "Type"
  | DB  (_,x,n)        -> fprintf fmt "%a[%i]" pp_ident x n
  | Const (_,n)        -> fprintf fmt "%a" pp_name n
  | App (f,a,args)     -> pp_list " " pp_term_wp fmt (f::a::args)
  | Lam (_,x,None,f)   -> fprintf fmt "%a => %a" pp_ident x pp_term f
  | Lam (_,x,Some a,f) -> fprintf fmt "%a:%a => %a" pp_ident x pp_term_wp a pp_term f
  | Pi  (_,x,a,b)      -> fprintf fmt "%a:%a -> %a" pp_ident x pp_term_wp a pp_term b

and pp_term_wp fmt te =
  match te with
  | Kind | Type _ | DB _ | Const _ as t -> pp_term fmt t
  | t                                  -> fprintf fmt "(%a)" pp_term t

let rec get_loc (te:term) : loc =
  match te with
  | Type l | DB (l,_,_) | Const (l,_) | Lam (l,_,_,_) | Pi (l,_,_,_)  -> l
  | Kind -> dloc
  | App (f,_,_) -> get_loc f

let mk_Kind             = Kind
let mk_Type l           = Type l
let mk_DB l x n         = DB (l,x,n)
let mk_Const l n        = Const (l,n)
let mk_Lam l x a b      = Lam (l,x,a,b)
let mk_Pi l x a b       = Pi (l,x,a,b)
let mk_Arrow l a b      = Pi (l,dmark,a,b)

let mk_App f a1 args =
  match f with
    | App (f',a1',args') -> App (f',a1',args'@(a1::args))
    | _ -> App(f,a1,args)

let rec term_eq t1 t2 =
  (* t1 == t2 || *)
  match t1, t2 with
    | Kind, Kind | Type _, Type _ -> true
    | DB (_,_,n), DB (_,_,n') -> n==n'
    | Const (_,cst), Const (_,cst') -> name_eq cst cst'
    | App (f,a,l), App (f',a',l') ->
        ( try List.for_all2 term_eq (f::a::l) (f'::a'::l')
          with _ -> false )
    | Lam (_,_,a,b), Lam (_,_,a',b') -> term_eq b b'
    | Pi (_,_,a,b), Pi (_,_,a',b') -> term_eq a a' && term_eq b b'
    | _, _  -> false


type untyped_context = ( loc * ident ) list

type typed_context = ( loc * ident * term ) list

let rec get_name_from_typed_ctxt ctxt i =
  try Some(List.nth ctxt i)
  with Failure _ -> None

let rename_vars_with_typed_context ctxt t =
  let rec aux ctxt d = function
    | DB  (l,v,n)        ->
      let v' = (match get_name_from_typed_ctxt ctxt (n - d) with Some v -> v | None -> v) in
      mk_DB l v' n
    | App (f,a,args)     ->
      mk_App (aux ctxt d f) (aux ctxt d a) (List.map (aux ctxt d) args)
    | Lam (l,x,None,f)   -> mk_Lam l x None (aux (x::ctxt) (d+1) f)
    | Lam (l,x,Some a,f) -> mk_Lam l x (Some (aux ctxt d a)) (aux (x::ctxt) (d+1) f)
    | Pi  (l,x,a,b)      -> mk_Pi l x (aux ctxt d a) (aux (x::ctxt) (d+1) b)
    | te -> te in
  aux (List.map (fun (_,v,_) -> v) ctxt) 0 t
