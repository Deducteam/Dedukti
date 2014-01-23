
type yes_no_maybe = Yes | No | Maybe
type 'a option2 = None2 | Maybe2 | Some2 of 'a

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
  | COMMAND   of ( loc * string )

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
  | Meta  of int

let mk_Kind             = Kind
let mk_Type             = Type
let mk_DB x n           = DB (x,n)
let mk_Const m v        = Const (m,v)
let mk_Lam x a b        = Lam (x,a,b)
let mk_Pi x a b         = Pi (x,a,b)
let mk_Meta n           = Meta n

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
    | DB (_,n), DB (_,n') 
    | Meta n, Meta n'                   -> n=n' 
    | Const (m,v), Const (m',v')        -> ident_eq v v' && ident_eq m m'
    | App l, App l'                     -> ( try List.for_all2 term_eq l l' with _ -> false )
    | Lam (_,a,b), Lam (_,a',b')
    | Pi (_,a,b), Pi (_,a',b')          -> term_eq a a' && term_eq b b'
    | _, _                              -> false

(* *** Rewrite Rules *** *)

type pattern =
  | Var         of ident*int
  | Joker       of int
  | Pattern     of ident*ident*pattern array
  | Dot         of term

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
exception MiscError of loc*string
exception EndOfFile
