
(* *** Identifiers (hashconsed strings) *** *)

type ident = string
let string_of_ident s = s
let ident_eq s1 s2 = s1==s2 || s1=s2
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

(* *** Localization *** *)

type loc = int*int
let dloc = (0,0)
let mk_loc l c = (l,c)
let of_loc l = l

(* *** Parsing *** *)

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

exception EndOfFile

(* *** Pseudo Terms *** *)

type preterm = (*FIXME need not to be private*)
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm * preterm
  | PrePi   of loc * ident option * preterm * preterm

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list

let rec get_loc = function
  | PreType l | PreId (l,_) | PreQId (l,_,_) | PreLam  (l,_,_,_)
  | PrePi   (l,_,_,_) -> l
  | PreApp (f,_,_) -> get_loc f

let mk_pre_type lc          = PreType lc
let mk_pre_id lc id         = PreId (lc,id)
let mk_pre_qid lc md id     = PreQId (lc,md,id)
let mk_pre_lam lc x ty te   = PreLam (lc,x,ty,te)
let mk_pre_arrow a b        = PrePi (get_loc a,None,a,b)
let mk_pre_pi lc x a b      = PrePi (lc,Some x,a,b)
let mk_pre_app f a1 args    = PreApp (f,a1,args)

let mk_pre_from_list = function
  | [] -> assert false
  | [t] -> t
  | f::a1::args -> PreApp (f,a1,args)

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = loc * pdecl list * ident * prepattern list * preterm

(* *** Terms *** *)

type term =
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | DB    of loc*ident*int              (* deBruijn *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*ident*term*term        (* Lambda abstraction *)
  | Pi    of loc*ident option*term*term (* Pi abstraction *)
  | Meta  of loc*int

let mk_Kind             = Kind
let mk_Type l           = Type l
let mk_DB l x n         = DB (l,x,n)
let mk_Const l m v      = Const (l,m,v)
let mk_Lam l x a b      = Lam (l,x,a,b)
let mk_Pi l x a b       = Pi (l,x,a,b)
let mk_Meta l n         = Meta (l,n)

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
    | Lam (_,_,a,b), Lam (_,_,a',b')
    | Pi (_,_,a,b), Pi (_,_,a',b')      -> term_eq a a' && term_eq b b'
    | _, _                              -> false

(* *** Rewrite Rules *** *)

type pattern = (*FIXME ajouter Joker*)
  | Var         of loc*ident*int
  | Pattern     of loc*ident*ident*pattern list
  | Brackets    of term

let rec term_of_pattern = function
  | Var (l,id,n) -> DB (l,id,n)
  | Brackets t -> t
  | Pattern (l,md,id,[]) -> Const (l,md,id)
  | Pattern (l,md,id,a::args) ->
      mk_App (Const (l,md,id)) (term_of_pattern a) (List.map term_of_pattern args)

type top = ident*pattern array
type context = ( ident * term ) list

type rule = {
        l:loc;
        ctx:context;
        id:ident;
        args:pattern list;
        rhs:term; }

type pattern2 =
  | Var2         of ident*int
  | Pattern2     of ident*ident*pattern2 array

type rule2 =
    { loc:loc ; pats:pattern2 array ; right:term ;
      constraints:(term*term) list ; env_size:int ; }

type dtree =
  | Switch      of int * (int*ident*ident*dtree) list * dtree option
  | Test        of (term*term) list * term * dtree option

type rw_infos =
  | Decl    of term
  | Def     of term*term
  | Decl_rw of term*rule2 list*int*dtree

(* Misc *)

type yes_no_maybe = Yes | No | Maybe
type 'a option2 = None2 | DontKnow | Some2 of 'a
type ('a,'b) sum = Success of 'a | Failure of 'b

(* Commands *)

type command =
  (* Reduction *)
  | Whnf of preterm
  | Hnf of preterm
  | Snf of preterm
  | OneStep of preterm
  | Conv of preterm*preterm
  (*Tying*)
  | Check of preterm*preterm
  | Infer of preterm
  (* Misc *)
  | Gdt of ident*ident
  | Print of ident
  | Other of string*preterm list
