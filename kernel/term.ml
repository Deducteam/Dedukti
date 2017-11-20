open Basic
open Format

module IntSet = Set.Make(struct type t = int let compare = compare end)

(** {2 Terms/Patterns} *)

type term =
  | Kind                                             (* Kind *)
  | Type  of loc                                     (* Type *)
  | DB    of loc * ident * int                       (* deBruijn *)
  | Const of loc * name                              (* Global variable *)
  | App   of term * term * term list                 (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc * ident * term * term        (* Lambda abstraction *)
  | Pi    of loc * ident * term * term               (* Pi abstraction *)
  | Meta  of loc * ident * int * term option ref

let rec pp_term fmt te =
  match te with
  | Kind               -> fprintf fmt "Kind"
  | Type _             -> fprintf fmt "Type"
  | DB  (_,x,n)        -> fprintf fmt "%a[%i]" pp_ident x n
  | Const (_,n)        -> fprintf fmt "%a" pp_name n
  | App (f,a,args)     -> pp_list " " pp_term_wp fmt (f::a::args)
  | Lam (_,x,a,f)      -> fprintf fmt "%a:%a => %a" pp_ident x pp_term_wp a pp_term f
  | Pi  (_,x,a,b)      -> fprintf fmt "%a:%a -> %a" pp_ident x pp_term_wp a pp_term b
  | Meta  (_,x,n,mt) ->
    match !mt with
    | None -> fprintf fmt "?%a[%d]" pp_ident x n
    | Some te -> fprintf fmt "%a" pp_term te

and pp_term_wp fmt te =
  match te with
  | Kind | Type _ | DB _ | Const _ as t -> pp_term fmt t
  | t                                  -> fprintf fmt "(%a)" pp_term t

let rec get_loc (te:term) : loc =
  match te with
  | Type l | DB (l,_,_) | Const (l,_) | Lam (l,_,_,_) | Pi (l,_,_,_) | Meta(l,_,_,_) -> l
  | Kind -> dloc
  | App (f,_,_) -> get_loc f

let meta_uniq_id = ref IntSet.empty
let current = ref 0

let mk_Kind             = Kind
let mk_Type l           = Type l
let mk_DB l x n         = DB (l,x,n)
let mk_Const l n        = Const (l,n)
let mk_Lam l x a b      = Lam (l,x,a,b)
let mk_Pi l x a b       = Pi (l,x,a,b)
let mk_Arrow l a b      = Pi (l,qmark,a,b)

let rec new_fresh_meta_id () =
  if IntSet.mem !current  !meta_uniq_id then
    begin
      incr current;
      new_fresh_meta_id ()
    end
  else
    begin
      meta_uniq_id := IntSet.add !current !meta_uniq_id;
      !current
    end

let mk_Meta l id =
    let c = new_fresh_meta_id () in
    Meta(l, id, c, ref None)

let mk_Meta2 l id n =
  meta_uniq_id := IntSet.add n !meta_uniq_id;
  Meta(l, id, n, ref None)


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
    | Meta(_,_,n,_), Meta(_,_,n',_) -> n==n'
    | _, _  -> false



type var = ident

type mvar

(* TODO: the same as Rule.untyped_context *)
type ctx = ( loc * ident ) list

type mctx = (loc * ident) list

type box_term =
  | MT of loc * ctx * term (* [ g |- term] *)
  | CT of loc * ctx (* [g] *)

type mtype =
  | Impl of loc * mtype * mtype (* mty -> mty' *)
  | Forall of loc * var * box_term * mtype (* forall m : [g|- term], mty *)
  | BoxTy of  box_term (* [g |- term] *)

type mterm =
  | MLamF of loc * ident * box_term * mterm (* x : [g|-term] => mte *)
  | MLamI of loc * ident * mterm * mterm (* e : mt => mte *)
  | BoxTe of box_term (* [g |- term] *)
  | Var of loc * var (* x *)
  | MApp of mterm * mterm (* f x *)
