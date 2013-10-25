
(* *** Hashconsed strings *** *)

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

(* *** Localisation *** *)

type loc = int*int
let dloc = (0,0)
let mk_loc l c = (c,l)
let string_of_loc (l,c) = "[l:" ^ string_of_int l ^ ";c:" ^ string_of_int c ^ "]"

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
  | PId   of loc*string
  | PQid  of loc*string*string
  | PApp  of pterm * pterm
  | PLam  of (loc*string) * pterm option * pterm
  | PPi   of (loc*string) option * pterm * pterm

type ppattern = 
  | PDash
  | PPat of (loc*string*string) * ppattern array 

type ptop = (loc*string) * ppattern array 

type pcontext = ( (loc*string) * pterm ) list

type prule  = pcontext * ptop * pterm (* [ env ] top_pattern --> term *)

(* *** Typing *** *)

type term = 
  | Kind
  | Type                        (* Type *)
  | DB   of int                 (* deBruijn *)
  | GVar of string*string       (* Global variable *)
  | App  of term list           (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam  of term*term           (* Lambda abstraction *)
  | Pi   of term*term           (* Pi abstraction *)
  | Meta of int

let mk_kind     = Kind
let mk_type     = Type
let mk_db n     = DB n
let mk_gvar m v = GVar (m,v)
let mk_lam a b  = Lam (a,b) 
let mk_pi a b   = Pi (a,b)
let mk_meta n   = Meta n
let mk_uapp lst = App lst
let mk_app = function
  | [] | [_]            -> assert false
  | (App l1)::l2        -> App (l1@l2)
  | lst                 -> App lst
let cpt = ref (-1)
let mk_unique _ = incr cpt ; GVar ( empty , hstring (string_of_int !cpt) )


(* *** Pattern matching *** *)

type pattern =
  | Var         of int
  | Dash        of int
  | Pattern     of (loc*string*string) * pattern array

type top = (loc*string*string) * pattern array 

type rule = int * top * term 

type gdt =
  | Leaf     of term
  | Switch   of int * ((string*string)*gdt) list * gdt option

(* *** Errors *** *)

exception ParserError of loc*string
exception LexerError  of loc*string
exception EnvError    of loc*string
exception TypingError of loc*string
exception PatternError of loc*string


