
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
  | ASSERT      of loc
  | EQUIV

(* Pseudo Terms *)

type pterm = private
  | P_Type of loc
  | P_Id   of loc * ident
  | P_QId  of loc * ident * ident
  | P_App  of pterm list
  | P_Lam  of loc * ident * pterm * pterm
  | P_Pi   of (loc*ident) option * pterm * pterm
  | P_Unknown of loc * int

val mk_type   : loc -> pterm
val mk_id     : loc -> ident -> pterm
val mk_qid    : loc -> ident -> ident -> pterm
val mk_lam    : loc -> ident -> pterm -> pterm -> pterm
val mk_app    : pterm list -> pterm
val mk_arrow  : pterm -> pterm -> pterm
val mk_pi     : loc -> ident -> pterm -> pterm -> pterm
val mk_unknown: loc -> pterm

val get_loc : pterm -> loc

type pdecl      = loc * ident * pterm
type pcontext   = pdecl list
type prule      = pcontext * pterm * pterm

(* *** Terms *** *)

type term = private
  | Kind                                (* Kind *)
  | Type                                (* Type *)
  | DB    of ident*int                  (* deBruijn *)
  | Const of ident*ident                (* Global variable *)
  | App   of term list                  (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam   of ident*term*term            (* Lambda abstraction *)
  | Pi    of ident option*term*term     (* Pi abstraction *)
  | Meta  of int

val term_eq : term -> term -> bool      (* Syntactic equality / Alpha-equivalence *)

val mk_Kind     : term
val mk_Type     : term
val mk_DB       : ident -> int -> term
val mk_Const    : ident -> ident -> term
val mk_Lam      : ident -> term -> term -> term
val mk_App      : term list -> term
val mk_Pi       : ident option -> term -> term -> term
val mk_Unique   : unit -> term
val mk_Meta     : int -> term

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

exception ParserError  of loc*string
exception LexerError   of loc*string
exception EnvError     of loc*string
exception TypingError  of loc*string
exception PatternError of loc*string
exception EndOfFile
