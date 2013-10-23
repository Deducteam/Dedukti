
module StringH = Hashtbl.Make(struct type t = string let equal a b = a = b let hash = Hashtbl.hash end)
module IntH    = Hashtbl.Make(struct type t = int let equal a b = a = b let hash = Hashtbl.hash end)

(* *** Parsing *** *)

type loc  = int*int

type token = 
  | UNDERSCORE of loc
  | TYPE
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | QID of (loc*string*string)
  | NORM
  | NAME
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | IMPORT
  | ID of (loc*string)
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | ARROW

type pterm =
  | PType
  | PId  of loc*string
  | PQid of loc*string*string
  | PApp of pterm * pterm
  | PLam of (loc*string) * pterm option * pterm
  | PPi  of (loc*string) option * pterm * pterm

type ppattern = 
  | PDash
  | PPat of (loc*string*string) * ppattern array (*FIXME array?*)

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

(* *** Pattern matching *** *)

type pattern =
  | Var         of int
  | Dash        of int
  | Pattern     of (string*string) * pattern array

type top = (string*string) * pattern array 

type rule = { li:pattern array ; te:term ; na:int array ; }

type pMat = rule array

type gdt =
  | Leaf     of term
  | Switch   of int * ((string*string)*gdt) list * gdt option

(* *** Errors *** *)

exception ParserError of loc*string
exception LexerError  of loc*string
exception EnvError    of string
exception TypingError of string
exception PatternError of string


