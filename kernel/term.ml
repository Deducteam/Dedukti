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
  | Let   of loc*ident*term*term        (* let x=a in b *)

type context = ( loc * ident * term ) list

let rec get_loc = function
  | Type l | DB (l,_,_) | Const (l,_,_) | Lam (l,_,_,_)
  | Let (l,_,_,_) | Pi (l,_,_,_)  -> l
  | Kind -> dloc
  | App (f,_,_) -> get_loc f

let mk_Kind             = Kind
let mk_Type l           = Type l
let mk_DB l x n         = DB (l,x,n)
let mk_Const l m v      = Const (l,m,v)
let mk_Lam l x a b      = Lam (l,x,a,b)
let mk_Pi l x a b       = Pi (l,x,a,b)
let mk_Arrow l a b      = Pi (l,qmark,a,b)
let mk_Let l x a b      = Let (l,x,a,b)

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
    | Let (_,_,a,b), Let (_,_,a',b')
    | Pi (_,_,a,b), Pi (_,_,a',b') -> term_eq a a' && term_eq b b'
    | _, _  -> false

(** {2 Let-bindings} *)

module LetCtx = struct
  type t = {
    env : (term * t) option LList.t;
  }

  let empty = {env=LList.nil}
  let is_empty {env} = LList.is_empty env
  let cons (t,e) {env} = {env=LList.cons (Some (t,e)) env}
  let cons_none {env} = {env=LList.cons None env}
  let nth {env} n = LList.nth env n
  let lst {env} = LList.lst env
  let len {env} = LList.len env

  let mem e n =
    len e > n && match nth e n with Some _ -> true | None -> false

  let get e n =
    if n >= LList.len e.env then None else nth e n

  let has_bindings {env} =
    List.exists (function None -> false | Some _ -> true) (LList.lst env)
end
