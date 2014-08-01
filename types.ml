(** {2 Identifiers (hashconsed strings)} *)

type ident = string
let string_of_ident s = s
let ident_eq s1 s2 = s1==s2 || s1=s2
let ident_cmp s1 s2 = String.compare s1 s2
let pp_ident = output_string

module WS = Weak.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

let shash       = WS.create 251
let hstring     = WS.merge shash
let empty       = hstring ""

let gensym =
  let n = ref 0 in
  fun () ->
    let s = hstring (Printf.sprintf "$x%d" !n) in
    incr n;
    s

(** {2 Lists with Length} *)

module LList = struct
  type 'a t= {
    len : int;
    lst : 'a list;
  }

  let cons x {len;lst} = {len=len+1; lst=x::lst}
  let nil = {len=0;lst=[];}
  let len x = x.len
  let lst x = x.lst
  let is_empty x = x.len = 0

  let make ~len lst =
    assert (List.length lst = len);
    {lst;len}

  let make_unsafe ~len lst = {len;lst}

  let map f {len;lst} = {len; lst=List.map f lst}
  let append_l {len;lst} l = {len=len+List.length l; lst=lst@l}

  let nth l i = List.nth l.lst i

  let remove i {len;lst} =
    let rec aux c lst = match lst with
      | []        -> assert false
      | x::lst'   -> if c==0 then lst' else x::(aux (c-1) lst')
    in
    {len=len-1; lst=aux i lst}
end

(** {2 Localization} *)

type loc = int*int
let dloc = (0,0)
let mk_loc l c = (l,c)
let of_loc l = l

(** {2 Parsing} *)

type token =
  | UNDERSCORE  of loc
  | TYPE        of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | QID         of ( loc * ident * ident )
  | NAME        of ( loc * ident )
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | ID          of ( loc * ident )
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | ARROW
  | WHNF        of loc
  | HNF         of loc
  | SNF         of loc
  | STEP        of loc
  | INFER       of loc
  | CONV        of loc
  | CHECK       of loc
  | PRINT       of loc
  | GDT         of loc
  | OTHER       of ( loc * string )
  | STRING      of string

exception EndOfFile

(** {2 PreTerms/PrePatterns} *)

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm * preterm
  | PrePi   of loc * ident option * preterm * preterm

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list
  | PJoker       of loc

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = loc * pdecl list * ident * prepattern list * preterm

(** {2 Terms/Patterns} *)

type term =
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | DB    of loc*ident*int              (* deBruijn *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*ident*term*term        (* Lambda abstraction *)
  | Pi    of loc*ident option*term*term (* Pi abstraction *)
  | Meta  of loc*int
  | Let   of loc*ident*term*term        (* let x=a in b *)

let rec get_loc = function
  | Kind -> dloc
  | Type l | DB (l,_,_) | Const (l,_,_)
  | Lam (l,_,_,_) | Pi (l,_,_,_) | Let (l, _, _, _) | Meta (l,_) -> l
  | App (f,_,_) -> get_loc f

let mk_Kind             = Kind
let mk_Type l           = Type l
let mk_DB l x n         = DB (l,x,n)
let mk_Const l m v      = Const (l,m,v)
let mk_Lam l x a b      = Lam (l,x,a,b)
let mk_Pi l x a b       = Pi (l,x,a,b)
let mk_Meta l n         = Meta (l,n)
let mk_Let l x a b      = Let (l,x,a,b)

let mk_App f a1 args =
  match f with
    | App (f',a1',args') -> App (f',a1',args'@(a1::args))
    | _ -> App(f,a1,args)

let cpt = ref (-1)
let mk_Unique _ =
  incr cpt ;
  Const ( dloc , empty , hstring (string_of_int !cpt) )

let rec term_eq t1 t2 =
  (* t1 == t2 || *)
  match t1, t2 with
    | Kind, Kind | Type _, Type _       -> true
    | DB (_,_,n), DB (_,_,n')
    | Meta (_,n), Meta (_,n')           -> n=n'
    | Const (_,m,v), Const (_,m',v')    -> ident_eq v v' && ident_eq m m'
    | App (f,a,l), App (f',a',l')       ->
        ( try List.for_all2 term_eq (f::a::l) (f'::a'::l')
          with _ -> false )
    | Let (_,_,a,b), Let (_,_,a',b')
    | Lam (_,_,a,b), Lam (_,_,a',b')
    | Pi (_,_,a,b), Pi (_,_,a',b')      -> term_eq a a' && term_eq b b'
    | _, _                              -> false

(* c <?> (f,x,y) is c if c<>0, else it is f x y. This is used
    for lexicographic combination *)
let (<?>) c (f,x,y) = if c=0 then f x y else c

let term_compare_depth (t1,i1) (t2,i2) =
  let to_int = function
    | Kind -> 0
    | Type _ -> 1
    | DB _ -> 2
    | Const _ -> 3
    | App _ -> 4
    | Lam _ -> 5
    | Pi _ -> 6
    | Meta _ -> 7
    | Let _ -> 8
  in
  let rec aux k t1 t2 = match t1, t2 with
    | Kind, Kind | Type _, Type _       -> 0
    | DB (_,_,n), DB (_,_,n')           ->
        (* unshift only if it's above k *)
        let n = if n<k then n else n-i1
        and n' = if n'<k then n' else n'-i2 in
        Pervasives.compare n n'
    | Meta (_,n), Meta (_,n')           -> Pervasives.compare n n'
    | Const (_,m,v), Const (_,m',v')    -> ident_cmp v v' <?> (ident_cmp, m, m')
    | App (f,a,l), App (f',a',l')       ->
        aux k f f' <?> (aux k, a, a') <?> (aux_list k, l, l')
    | Lam (_,_,a,b), Lam (_,_,a',b')
    | Let (_,_,a,b), Let (_,_,a',b')
    | Pi (_,_,a,b), Pi (_,_,a',b')      -> aux k a a' <?> (aux (k+1), b, b')
    | _, _                              -> Pervasives.compare (to_int t1) (to_int t2)
  and aux_list k l1 l2 = match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | t1::tl1, t2::tl2 -> aux k t1 t2 <?> (aux_list k, tl1, tl2)
  in
  aux 0 t1 t2

let term_equal_depth a b = term_compare_depth a b=0

type pattern =
  | Var         of loc*ident*int
  | Pattern     of loc*ident*ident*pattern list
  | Brackets    of term
  | Joker       of loc*int

type top = ident*pattern array
type context = ( ident * term ) list

(**{2 Rewrite Rules} *)

type rule = {
        l:loc;
        ctx:context;
        md:ident;
        id:ident;
        args:pattern list;
        rhs:term; }

type dtree =
  | Switch      of int * (int*ident*ident*dtree) list * dtree option
  | Test        of (term*term) list * term * dtree option

(** {2 Environment} *)

module H = Hashtbl.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

type rw_infos =
  | Decl    of term
  | Def     of term*term
  | Decl_rw of term*rule list*int*dtree

(** {2 Commands} *)

type command =
  (* Reduction *)
  | Whnf of preterm
  | Hnf of preterm
  | Snf of preterm
  | OneStep of preterm
  | Conv of preterm*preterm
  (*Typing*)
  | Check of preterm*preterm
  | Infer of preterm
  (* Misc *)
  | Gdt of ident*ident
  | Print of string
  | Other of string*preterm list
