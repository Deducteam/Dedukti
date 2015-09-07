open Basics
open Multi_set

(** {2 Terms/Patterns} *)

type term =
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | DB    of loc*ident*int              (* deBruijn *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*ident*term option*term        (* Lambda abstraction *)
  | Pi    of loc*ident*term*term (* Pi abstraction *)

type context = ( loc * ident * term ) list

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

let rec term_cmpr t1 t2 =
  match t1, t2 with
  | Kind, Kind | Type _, Type _ -> 0
  | DB (_,_,n), DB (_,_,n') -> compare n n'
  | Const (_,m,v), Const (_,m',v') ->
    let cmp = compare (string_of_ident m) (string_of_ident m') in
    if cmp = 0 then compare (string_of_ident v) (string_of_ident v')
    else cmp
  | App (f,a,l), App(f',a',l') ->
    let rec aux l1 l2 = match l1, l2 with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | h1::tl1, h2::tl2 -> 
        let cmp = term_cmpr h1 h2 in
        if cmp = 0 then aux tl1 tl2
        else cmp
    in aux (f::a::l) (f'::a'::l')
  | Lam (_,_,a,b), Lam (_,_,a',b') -> term_cmpr b b'
  | Pi (_,_,a,b), Pi (_,_,a',b') ->
    let cmp = term_cmpr a a' in
    if cmp = 0 then term_cmpr b b'
    else cmp
  | Kind, _ -> -1
  | Type _, Kind -> 1
  | Type _, _ -> -1
  | DB _, Kind | DB _, Type _ -> 1
  | DB _, _ -> -1
  | Const _, Kind | Const _, Type _ | Const _, DB _ -> 1
  | Const _, _ -> -1
  | App _, Kind | App _, Type _ | App _, DB _ | App _, Const _ -> 1
  | App _, _ -> -1
  | Lam _, Kind | Lam _, Type _ | Lam _, DB _ | Lam _, Const _ | Lam _, App _ -> 1
  | Lam _, _ -> -1
  | Pi _, _ -> 1

let rec flatten_add t = match t with
  | Const(_,m,v) when ident_eq v (hstring "0") -> []
  | App(f,a, [ b ]) ->
    begin
      match f with
      | Const(_, m, v) when ident_eq v (hstring "add") ->
        (flatten_add a) @ (flatten_add b)
      | _ -> []
    end
  | _ -> [ t ]

let rec term_eq t1 t2 =
  (* t1 == t2 || *)
  match t1, t2 with
    | Kind, Kind | Type _, Type _ -> true
    | DB (_,_,n), DB (_,_,n') -> n==n'
    | Const (_,m,v), Const (_,m',v') -> ident_eq v v' && ident_eq m m'
    | App (f,a,l), App (f',a',l') ->
     begin
       match f, f' with
       | Const(_,m,v), Const(_,m',v') when ((term_eq f f') && (ident_eq v (hstring "add"))) ->
         if (List.length (a::l)) = 2 && (List.length (a'::l')) = 2 then
           begin
             let m1 = mk_Multiset term_cmpr (flatten_add t1) in
             let m2 = mk_Multiset term_cmpr (flatten_add t2) in
             (cmpr_Multiset term_cmpr m1 m2) = 0
           end
         else assert false
       | _ ->
         ( try List.for_all2 term_eq (f::a::l) (f'::a'::l') with _ -> false )
     end
    | Lam (_,_,a,b), Lam (_,_,a',b') -> term_eq b b'
    | Pi (_,_,a,b), Pi (_,_,a',b') -> term_eq a a' && term_eq b b'
    | _, _  -> false

let rec pp_term out = function
  | Kind               -> output_string out "Kind"
  | Type _             -> output_string out "Type"
  | DB  (_,x,n)        -> Printf.fprintf out "%a[%i]" pp_ident x n
  | Const (_,m,v)      -> Printf.fprintf out "%a.%a" pp_ident m pp_ident v
  | App (f,a,args)     -> pp_list " " pp_term_wp out (f::a::args)
  | Lam (_,x,None,f)   -> Printf.fprintf out "%a => %a" pp_ident x pp_term f
  | Lam (_,x,Some a,f) -> Printf.fprintf out "%a:%a => %a" pp_ident x pp_term_wp a pp_term f
  | Pi  (_,x,a,b)      -> Printf.fprintf out "%a:%a -> %a" pp_ident x pp_term_wp a pp_term b

and pp_term_wp out = function
  | Kind | Type _ | DB _ | Const _ as t -> pp_term out t
  | t                                  -> Printf.fprintf out "(%a)" pp_term t

let pp_context out ctx =
  pp_list ".\n" (fun out (_,x,ty) ->
                   Printf.fprintf out "%a: %a" pp_ident x pp_term ty )
    out (List.rev ctx)
