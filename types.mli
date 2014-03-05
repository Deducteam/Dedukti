
(** This modules provides the basic types used in Dedukti *)

(** {2 Identifiers (hashconsed strings)} *)
(** Internal representation of identifiers as hashconsed strings. *)

type ident
val empty : ident
val string_of_ident : ident -> string
val hstring : string -> ident
val ident_eq : ident -> ident -> bool

(** {2 Localization} *)

type loc
val dloc                : loc
val mk_loc              : int -> int -> loc
val of_loc              : loc -> (int*int)

(** {2 Parsing} *)

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
  | ID          of ( loc * ident )
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | ARROW
  | COMMAND      of ( loc * string )

exception EndOfFile

(** {2 PreTerms/PrePatterns} *)

type preterm = private
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm list
  | PreLam  of loc * ident * preterm * preterm
  | PrePi   of (loc*ident) option * preterm * preterm

val mk_pre_type         : loc -> preterm
val mk_pre_id           : loc -> ident -> preterm
val mk_pre_qid          : loc -> ident -> ident -> preterm
val mk_pre_lam          : loc -> ident -> preterm -> preterm -> preterm
val mk_pre_app          : preterm list -> preterm
val mk_pre_arrow        : preterm -> preterm -> preterm
val mk_pre_pi           : loc -> ident -> preterm -> preterm -> preterm

val get_loc : preterm -> loc

type prepattern =
  | Unknown     of loc
  | PPattern    of loc*ident option*ident*prepattern list

type ptop = loc * ident * prepattern list
type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = pcontext * ptop * preterm

(** {2 Terms/Patterns} *)

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

type pattern =
  | Var         of ident option*int
  | Pattern     of ident*ident*pattern array

val term_of_pattern : pattern -> term
val term_of_pattern_all_meta : pattern -> term

type top = ident*pattern array
type context = ( ident * term ) list

(**{2 Rewrite Rules} *)

type rule = {
  nb:int;
  md:ident;
  l:loc;
  ctx:context;
  id:ident;
  args:pattern array;
  ri:term;
  sub:(int*term) list;
  k:int;
}
(* FIXME
type cpair = {
  rule1:int;
  rule2:int;
  pos:int list;
  root:pattern;
  red1:term;
  red2:term;
  joinable:bool
}
 *)
type gdt =
  | Switch      of int * ((ident*ident)*gdt) list * gdt option
  | Test        of (term*term) list*term*gdt option

(** {2 Commands} *)

(* FIXME mettre dans le parser ? *)
type cmd =
  (* Reduction *)
  | Whnf of preterm
  | Hnf of preterm
  | Snf of preterm
  | OneStep of preterm
  | Conv of preterm*preterm
  (*Typing*)
  | Check of preterm*preterm
  | Infer of preterm
  (* Misc *)
  | Gdt of ident*ident
  | Print of string
  | Other

(** {2 Misc} *)

type yes_no_maybe = Yes | No | Maybe
type 'a option2 = None2 | DontKnow | Some2 of 'a
type ('a,'b) sum = Success of 'a | Failure of 'b
