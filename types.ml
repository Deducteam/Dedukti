
module StringH = Hashtbl.Make(struct type t = string let equal a b = a = b let hash = Hashtbl.hash end)
module IntH    = Hashtbl.Make(struct type t = int let equal a b = a = b let hash = Hashtbl.hash end)

(* Errors *)

exception ParserError of string
exception LexerError  of string
exception EnvError    of string
exception TypingError of string
exception PatternError of string

(* Parsing *)

type loc  = int*int

type pterm =
  | PType
  | PId  of loc*string
  | PQid of loc*string*string
  | PApp of pterm * pterm
  | PLam of (loc*string) * pterm option * pterm
  | PPi  of (loc*string) option * pterm * pterm

type pattern = 
  | Pat of (loc*string*string) * pterm array * pattern array

type top_pattern = (loc*string) * pterm array * pattern array 

type context = ( (loc*string) * pterm ) list

type rule  = context * top_pattern * pterm (* [ env ] top_pattern --> term *)

(* Typing *)

type term = 
  | Kind
  | Type                        (* Type *)
  | DB   of int                 (* deBruijn *)
  | GVar of string*string       (* Global variable *)
  | LVar of int                 (* Local variable (only for conversion test)*)
  | App  of term list           (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam  of term*term           (* Lambda abstraction *)
  | Pi   of term*term           (* Pi abstraction *)

(* Pattern matching *)

type rule2 = string list*top_pattern*term

type pattern2 =
  | Joker
  | Var of string
  | Pattern of (string*string) * pattern2 array

type line = { li:pattern2 array ; te:term ; na:string list ; }
type pMat = line array

type gdt =
  | Leaf     of term
  | Switch   of int * ((string*string)*gdt) list * gdt option

