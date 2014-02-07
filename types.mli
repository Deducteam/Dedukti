
type yes_no_maybe = Yes | No | Maybe
type 'a option2 = None2 | Maybe2 | Some2 of 'a

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
val string_of_loc       : loc -> string

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
  | COMMAND      of ( loc * string )

(* Pre Terms *)

type preterm = private
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

val mk_pre_type         : loc -> preterm
val mk_pre_id           : loc -> ident -> preterm
val mk_pre_qid          : loc -> ident -> ident -> preterm
val mk_pre_lam          : loc -> ident -> preterm -> preterm -> preterm
val mk_pre_app          : preterm list -> preterm
val mk_pre_arrow        : preterm -> preterm -> preterm
val mk_pre_pi           : loc -> ident -> preterm -> preterm -> preterm

val mk_unknown          : loc -> prepattern

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = pcontext * ptop * preterm

val get_loc : preterm -> loc

(* *** Terms *** *)

type term = private
  | Kind                 (* Kind *)
  | Type                 (* Type *)
  | DB    of ident*int   (* deBruijn *)
  | Const of ident*ident (* Global variable *)
  | App   of term list   (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam   of ident*term*term            (* Lambda abstraction *)
  | Pi    of ident option*term*term     (* Pi abstraction *)
  | Meta  of int

val mk_Kind     : term
val mk_Type     : term
val mk_DB       : ident -> int -> term
val mk_Const    : ident -> ident -> term
val mk_Lam      : ident -> term -> term -> term
val mk_App      : term list -> term
val mk_Pi       : ident option -> term -> term -> term
val mk_Unique   : unit -> term
val mk_Meta     : int -> term

(* Syntactic equality / Alpha-equivalence *)
val term_eq : term -> term -> bool      

(* *** Rewrite Rules *** *)

type pattern = 
  | Var         of ident*int
  | Joker       of int
  | Pattern     of ident*ident*pattern array
  | Dot         of term

type top = ident*pattern array
type context = ( ident * term ) list
type rule = { l:loc; ctx:context;  id:ident; args:pattern array; ri:term; } 

type gdt =
  | Switch      of int * ((ident*ident)*gdt) list * gdt option
  | Test        of (term*term) list*term*gdt option

(* *** Errors *** *)

exception ParserError  of loc*string
exception LexerError   of loc*string
exception EnvError     of loc*string
exception TypingError  of loc*string
exception PatternError of loc*string
exception MiscError of loc*string
exception EndOfFile
