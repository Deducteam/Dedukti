
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

type pterm =
  | P_Type of loc
  | P_Id   of loc * ident
  | P_QId  of loc * ident * ident
  | P_App  of pterm list
  | P_Lam  of loc * ident * pterm * pterm
  | P_Pi   of (loc*ident) option * pterm * pterm
  | P_Unknown of loc * int

let mk_type lc          = P_Type lc
let mk_id lc id         = P_Id (lc,id)
let mk_qid lc md id     = P_QId (lc,md,id)
let mk_lam lc x ty te   = P_Lam (lc,x,ty,te)
let mk_arrow a b        = P_Pi (None,a,b)
let mk_pi lc x a b      = P_Pi (Some(lc,x),a,b)
let mk_app              = function
  | []                  -> assert false
  | [t]                 -> t
  | (P_App l1)::l2      -> P_App (l1@l2)
  | lst -> P_App lst
let meta = ref (-1)
let mk_unknown lc =
  incr meta ; P_Unknown (lc,!meta)

let rec get_loc = function
  | P_Type l | P_Id (l,_) | P_QId (l,_,_)
  | P_Lam  (l,_,_,_) | P_Pi   (Some(l,_),_,_)
  | P_Unknown (l,_)                     -> l
  | P_Pi   (None,f,_) | P_App (f::_)    -> get_loc f
  | P_App _                             -> assert false

type ptop       = (loc*ident) * pterm list
type pdecl      = loc * ident * pterm
type pcontext   = pdecl list
type prule      = pcontext * pterm * pterm

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

type context = ( ident * term ) list
type rule = loc * int * ident * pattern array * term * (term*term) list

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
