
(* *** Identifiers (hashconsed strings) *** *)

type ident = string
let string_of_ident s = s
let ident_eq s1 s2 = s1==s2 || s1=s2

module WS = Weak.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

let shash       = WS.create 251
let hstring     = WS.merge shash
let empty     = hstring ""

(* *** Localization *** *)

type loc = int*int
let dloc = (0,0)
let mk_loc l c = (l,c)
let get_line (l,_) = l
let get_column (_,c) = c
let string_of_loc (l,c) = "line:" ^ string_of_int l ^ " column:" ^ string_of_int c

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
  | IMPORT      of ( loc * ident )
  | ID          of ( loc * ident )
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | ARROW
  | ASSERT      of loc
  | EQUIV

(* *** Pseudo Terms *** *)

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm list
  | PreLam  of loc * ident * preterm * preterm
  | PrePi   of (loc*ident) option * preterm * preterm

type prepattern = 
  | Unknown     of loc*int
  | PPattern    of loc*ident option*ident*prepattern list

type ptop = loc * ident * prepattern list

let mk_pre_type lc          = PreType lc
let mk_pre_id lc id         = PreId (lc,id)
let mk_pre_qid lc md id     = PreQId (lc,md,id)
let mk_pre_lam lc x ty te   = PreLam (lc,x,ty,te)
let mk_pre_arrow a b        = PrePi (None,a,b)
let mk_pre_pi lc x a b      = PrePi (Some(lc,x),a,b)
let mk_pre_app              = function
  | []                  -> assert false
  | [t]                 -> t
  | (PreApp l1)::l2      -> PreApp (l1@l2)
  | lst -> PreApp lst

let cpt = ref (-1)
let mk_unknown l = incr cpt ; Unknown (l,!cpt)

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = pcontext * ptop * preterm
 
let rec get_loc = function
  | PreType l | PreId (l,_) | PreQId (l,_,_)
  | PreLam  (l,_,_,_) | PrePi   (Some(l,_),_,_) -> l
  | PrePi   (None,f,_) | PreApp (f::_)          -> get_loc f
  | PreApp _                                    -> assert false

(* *** Terms *** *)

type term =
  | Kind                                (* Kind *)
  | Type                                (* Type *)
  | DB    of ident*int                  (* deBruijn *)
  | Const of ident*ident                (* Global variable *)
  | App   of term list                  (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam   of ident*term*term            (* Lambda abstraction *)
  | Pi    of ident option*term*term     (* Pi abstraction *)

let mk_Kind             = Kind
let mk_Type             = Type
let mk_DB x n           = DB (x,n)
let mk_Const m v        = Const (m,v)
let mk_Lam x a b        = Lam (x,a,b)
let mk_Pi x a b         = Pi (x,a,b)

let mk_App              = function
  | [] | [_] -> assert false
  | (App l1)::l2 -> App (l1@l2)
  | lst -> App lst

let cpt = ref (-1)
let mk_Unique _ =
  incr cpt ;
  Const ( empty , hstring (string_of_int !cpt) )

let rec term_eq t1 t2 =
  (* t1 == t2 || *)
  match t1, t2 with
    | Kind, Kind | Type , Type          -> true
    | DB (_,n), DB (_,n')               -> n=n' 
    | Const (m,v), Const (m',v')        -> ident_eq v v' && ident_eq m m'
    | App l, App l'                     -> ( try List.for_all2 term_eq l l' with _ -> false )
    | Lam (_,a,b), Lam (_,a',b')
    | Pi (_,a,b), Pi (_,a',b')          -> term_eq a a' && term_eq b b'
    | _, _                              -> false

(* *** Partial Terms *** *)

type partial_term = 
  | PartialApp  of partial_term list                  
  | PartialLam  of ident * partial_term * partial_term            
  | PartialPi   of ident option * partial_term * partial_term     
  | Meta        of int 
  | Term        of term

let mk_partial te = Term te
let mk_meta n = Meta n

let mk_partial_lam x ty te = 
 match ty,te with
   | Term ty', Term te' -> Term (Lam (x,ty',te'))
   | _, _               -> PartialLam (x,ty,te)

let mk_partial_pi x a b = 
  match a, b with
    | Term a', Term b'  -> Term (Pi (x,a',b'))
    | _, _              -> PartialPi (x,a,b)

let rec extract = function
  | []                  -> Some []
  | (Term t)::tl        -> 
      ( match extract tl with 
          | None -> None 
          | Some tl' -> Some (t::tl') )
  | _::_                -> None

let mk_partial_app lst = 
  match extract lst with
    | None      -> PartialApp lst
    | Some lst' -> Term (mk_App lst')

(* *** Rewrite Rules *** *)

type pattern =
  | Var         of ident*int
  | Joker       of int
  | Pattern     of ident*ident*pattern array
  | Dot         of partial_term

type top = ident*pattern array
type context = ( ident * term ) list
type rule = loc * context * ident * pattern array * term 

type gdt =
  | Switch      of int * ((ident*ident)*gdt) list * gdt option
  | Test        of (term*term) list*term*gdt option

(* *** Errors *** *)

exception ParserError of loc*string
exception LexerError  of loc*string
exception EnvError    of loc*string
exception TypingError of loc*string
exception PatternError of loc*string
exception EndOfFile
