open Basic
open Format

(** {2 Terms/Patterns} *)

type term =
  | Kind                                             (* Kind *)
  | Type  of loc                                     (* Type *)
  | DB    of loc * ident * int                       (* deBruijn *)
  | Const of loc * ident * ident                     (* Global variable *)
  | App   of term * term * term list                 (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc * ident * term option * term        (* Lambda abstraction *)
  | Pi    of loc * ident * term * term               (* Pi abstraction *)

let rec pp_term fmt te =
  match te with
  | Kind               -> fprintf fmt "Kind"
  | Type _             -> fprintf fmt "Type"
  | DB  (_,x,n)        -> fprintf fmt "%a[%i]" pp_ident x n
  | Const (_,m,v)      -> fprintf fmt "%a.%a" pp_ident m pp_ident v
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
  | Type l | DB (l,_,_) | Const (l,_,_) | Lam (l,_,_,_) | Pi (l,_,_,_)  -> l
  | Kind -> dloc
  | App (f,_,_) -> get_loc f

let mk_Kind             = Kind
let mk_Type l           = Type l
let mk_DB l x n         = DB (l,x,n)
let mk_Const l m v      = Const (l,m,v)
let mk_Lam l x a b      = Lam (l,x,a,b)
let mk_Pi l x a b       = Pi (l,x,a,b)
let mk_Arrow l a b      = Pi (l,qmark,a,b)

let mk_App f a1 args =
  match f with
    | App (f',a1',args') -> App (f',a1',args'@(a1::args))
    | _ -> App(f,a1,args)
let mk_App2 f = function
  | [] -> f
  | hd :: tl -> mk_App f hd tl

let rec term_eq t1 t2 =
  (* t1 == t2 || *)
  match t1, t2 with
    | Kind, Kind | Type _, Type _ -> true
    | DB (_,_,n), DB (_,_,n') -> n==n'
    | Const (_,m,v), Const (_,m',v') -> ident_eq v v' && ident_eq m m'
    | App (f,a,l), App (f',a',l') ->
        ( try List.for_all2 term_eq (f::a::l) (f'::a'::l')
          with _ -> false )
    | Lam (_,_,a,b), Lam (_,_,a',b') -> term_eq b b'
    | Pi (_,_,a,b), Pi (_,_,a',b') -> term_eq a a' && term_eq b b'
    | _, _  -> false

(** Type of ident comparison functions *)
type ident_comparator = ident -> ident -> ident -> ident -> int

(** Type of term comparison functions *)
type term_comparator = term -> term -> int

let rec compare_term id_comp t1 t2 =
  match t1, t2 with
  | Kind  , Kind   -> 0
  | Type _, Type _ -> 0
  | Const  (_,m,v), Const (_,m',v') -> id_comp m v m' v'
  | DB (_,_,n), DB (_,_,n') -> compare n n'
  | App (f,a,ar), App (f',a',ar') ->
     let c = compare_term id_comp f f' in
     if c <> 0 then c
     else
       let c = compare_term id_comp a a' in
       if c <> 0 then c
       else compare_term_list id_comp ar ar'
  | Lam (_,_,_,t), Lam (_,_,_,t') -> compare_term id_comp t t'
  | Pi (_,_,a,b), Pi (_,_,a',b') ->
     let c = compare_term id_comp a a' in
     if c = 0 then compare_term id_comp b b' else c
  | _, Kind    -> 1
  | Kind, _    -> -1
  | _, Type _  -> 1
  | Type _, _  -> -1
  | _, Const _ -> 1
  | Const _, _ -> -1
  | _, DB _    -> 1
  | DB _, _    -> -1
  | _, App _   -> 1
  | App _, _   -> -1
  | _, Lam _   -> 1
  | Lam _, _   -> -1

and compare_term_list id_comp a b =
  match a,b with
  | [], [] -> 0
  | l , [] -> 1
  | [], l  -> -1
  | h::t, h'::t' ->
     let c = compare_term id_comp h h' in
     if c = 0 then compare_term_list id_comp t t' else c
