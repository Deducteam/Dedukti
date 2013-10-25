
type ident
val string_of_ident : ident -> string
val ident_eq : ident -> ident -> bool

module WS :
sig 
  type t
  val create : int -> t
  val merge  : t -> string -> ident
end

(* *** String sharing *** *)

val hstring : string -> ident
val empty : ident

(* *** Parsing *** *)

type loc
val dloc                : loc
val mk_loc              : int -> int -> loc
val string_of_loc       : loc -> string 

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
  | PId   of loc * ident
  | PQid  of loc * ident * ident
  | PApp  of pterm * pterm
  | PLam  of (loc*ident) * pterm option * pterm
  | PPi   of (loc*ident) option * pterm * pterm

type ppattern = 
  | PDash
  | PPat of (loc*ident*ident) * ppattern array 

type ptop = (loc*ident) * ppattern array 

type pcontext = ( (loc*ident) * pterm ) list

type prule  = pcontext * ptop * pterm (* [ env ] top_pattern --> term *)

(* *** Typing *** *)

type term = private 
  | Kind
  | Type                        (* Type *)
  | DB   of int                 (* deBruijn *)
  | GVar of ident*ident         (* Global variable *)
  | App  of term list           (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam  of term*term           (* Lambda abstraction *)
  | Pi   of term*term           (* Pi abstraction *)
  | Meta of int

val mk_kind     : term
val mk_type     : term
val mk_db       : int -> term
val mk_gvar     : ident -> ident -> term
val mk_lam      : term -> term -> term
val mk_app      : term list -> term
val mk_uapp     : term list -> term
val mk_pi       : term -> term -> term
val mk_meta     : int -> term
val mk_unique   : unit -> term

(* *** Pattern matching *** *)

type pattern =
  | Var         of int
  | Dash        of int
  | Pattern     of (loc*ident*ident) * pattern array

type top = (loc*ident*ident) * pattern array 

type rule = int * top * term 

type gdt =
  | Leaf     of term
  | Switch   of int * ((ident*ident)*gdt) list * gdt option

(* *** Errors *** *)

exception ParserError  of loc*string
exception LexerError   of loc*string
exception EnvError     of loc*string
exception TypingError  of loc*string
exception PatternError of loc*string


