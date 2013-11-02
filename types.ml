
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

(* *** Parsing *** *)

type token = 
  | UNDERSCORE  of loc
  | TYPE        of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA 
  | QID         of ( loc * ident * ident )
  | NORM
  | NAME
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA 
  | IMPORT
  | ID          of ( loc * ident )
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | ARROW

type pterm =
  | PType of loc 
  | PId   of loc*ident
  | PQid  of loc*ident*ident
  | PApp  of pterm list
  | PLam  of (loc*ident) * pterm option * pterm
  | PPi   of (loc*ident) option * pterm * pterm

let mk_papp = function
  | []          -> assert false
  | [t]         -> t
  | args        -> PApp args

type ppattern = 
  | PDash of loc
  | PPat of (loc*ident*ident) * ppattern array 

type ptop = (loc*ident) * ppattern array 

(* *** Typing *** *)

type term = 
  | Kind
  | Type of loc                 
  | DB   of loc*ident*int       
  | GVar of loc*ident*ident     
  | App  of term list           
  | Lam  of loc*ident*term*term 
  | Pi   of loc*ident*term*term 
  | Meta of loc*int

let rec get_loc = function
  | Type l | DB (l,_,_) | GVar (l,_,_)| Lam  (l,_,_,_) 
  | Pi   (l,_,_,_) | Meta (l,_) -> l
  | App (f::_)                  -> get_loc f
  | App _                       -> assert false
  | Kind                        -> assert false

let mk_kind             = Kind
let mk_type l           = Type l
let mk_db l x n         = DB (l,x,n)
let mk_gvar l m v       = GVar (l,m,v)
let mk_lam l x a b      = Lam (l,x,a,b) 
let mk_pi l x a b       = Pi (l,x,a,b)
let mk_meta l n         = Meta (l,n)
let mk_uapp lst         = App lst
let mk_app              = function 
  | [] | [_] -> assert false 
  | (App l1)::l2 -> App (l1@l2) | lst -> App lst

let cpt = ref (-1)
let mk_unique _         = incr cpt ; GVar ( dloc , empty , hstring (string_of_int !cpt) )

let rec term_eq t1 t2 = 
  t1 == t2 || 
  match t1, t2 with 
    | Kind, Kind | Type _, Type _       -> true 
    | Meta (_,n), Meta (_,n') 
    | DB (_,_,n), DB (_,_,n')           -> n=n' 
    | GVar (_,m,v), GVar (_,m',v')      -> ident_eq v v' && ident_eq m m' 
    | App l, App l'                     -> ( try List.for_all2 term_eq l l' with _ -> false ) 
    | ( Lam (_,_,a,b), Lam (_,_,a',b') ) 
    | ( Pi (_,_,a,b), Pi (_,_,a',b') )  -> term_eq a a' && term_eq b b'
    | _, _                              -> false

(* *** Rewrite Rules *** *)

type pattern =
  | Var         of loc*ident*int
  | Dash        of loc*int
  | Pattern     of (loc*ident*ident) * pattern array

type top = ( loc * ident ) * pattern array 

type context = ( loc * ident * term ) list
                  
type rule = context * top * term 

type gdt =
  | Leaf     of term
  | Switch   of int * ((ident*ident)*gdt) list * gdt option

(* *** Errors *** *)

exception ParserError of loc*string
exception LexerError  of loc*string
exception EnvError    of loc*string
exception TypingError of loc*string
exception PatternError of loc*string
exception EndOfFile 
