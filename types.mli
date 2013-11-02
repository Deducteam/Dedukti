
(* *** Identifiers (hashconsed strings) *** *)

type ident
val string_of_ident : ident -> string
val ident_eq : ident -> ident -> bool

module WS :
sig 
  type t
  val create : int -> t
  val merge  : t -> string -> ident
end

val hstring : string -> ident
val empty : ident

(* *** Localization *** *)                          

type loc
val dloc                : loc
val mk_loc              : int -> int -> loc
val get_line            : loc -> int 
val get_column          : loc -> int 

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
  | PId   of loc * ident
  | PQid  of loc * ident * ident
  | PApp  of pterm list
  | PLam  of (loc*ident) * pterm option * pterm
  | PPi   of (loc*ident) option * pterm * pterm

val mk_papp : pterm list -> pterm

type ppattern = 
  | PDash of loc
  | PPat  of (loc*ident*ident) * ppattern array 

type ptop = (loc*ident) * ppattern array 

(* *** Typing *** *)

type term = private 
  | Kind                        (* Kind *)        
  | Type of loc                 (* Type *)
  | DB   of loc*ident*int       (* deBruijn *)
  | GVar of loc*ident*ident     (* Global variable *)
  | App  of term list           (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam  of loc*ident*term*term (* Lambda abstraction *)
  | Pi   of loc*ident*term*term (* Pi abstraction *)
  | Meta of loc*int             (* Meta variable (used to type rules) *)

val get_loc : term -> loc

val mk_kind     : term
val mk_type     : loc -> term
val mk_db       : loc -> ident -> int -> term
val mk_gvar     : loc -> ident -> ident -> term
val mk_lam      : loc -> ident -> term -> term -> term
val mk_app      : term list -> term
val mk_uapp     : term list -> term
val mk_pi       : loc -> ident -> term -> term -> term
val mk_meta     : loc -> int -> term
val mk_unique   : unit -> term
                            
val term_eq : term -> term -> bool (* Syntactic equality / Alpha-equivalence *)
 
(* *** Rewrite Rules *** *)

type pattern =
  | Var         of loc*ident*int
  | Dash        of loc*int
  | Pattern     of (loc*ident*ident) * pattern array

type top = (loc*ident) * pattern array 

type context = ( loc * ident * term ) list

type rule = context * top * term 

type gdt =
  | Leaf     of term
  | Switch   of int * ((ident*ident)*gdt) list * gdt option

(* *** Errors *** *)

exception ParserError  of loc*string
exception LexerError   of loc*string
exception EnvError     of loc*string
exception TypingError  of loc*string
exception PatternError of loc*string
exception EndOfFile 
