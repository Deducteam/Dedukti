open Basics

(** {2 Terms/Patterns} *)

type term =
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | DB    of loc*ident*int              (* deBruijn *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*ident*term option*term        (* Lambda abstraction *)
  | Pi    of loc*ident*term*term (* Pi abstraction *)

type context = ( ident * term ) list

let rec get_loc = function
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

(** {2 Commands} *)
(*
type command =
  (* Reduction *)
  | Whnf of preterm
  | Hnf of preterm
  | Snf of preterm
  | OneStep of preterm
  | Conv of preterm*preterm
  (* Typing *)
  | Check of preterm*preterm
  | Infer of preterm
  (* Misc *)
  | Gdt of ident option*ident
  | Print of string
  | Other of string*preterm list *)
