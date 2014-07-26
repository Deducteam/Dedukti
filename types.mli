
(** This modules provides the basic types used in Dedukti *)

(** {2 Identifiers (hashconsed strings)} *)
(** Internal representation of identifiers as hashconsed strings. *)

type ident
val empty : ident
val string_of_ident : ident -> string
val pp_ident : out_channel -> ident -> unit
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
  | WHNF        of loc
  | HNF         of loc
  | SNF         of loc
  | STEP        of loc
  | INFER       of loc
  | CONV        of loc
  | CHECK       of loc
  | PRINT       of loc
  | GDT         of loc
  | OTHER       of ( loc * string )
  | CHAR        of ( loc * char )
  | STRING      of ( loc * string )
  | NUM         of ( loc * string )

exception EndOfFile

(** {2 PreTerms/PrePatterns} *)

type preterm = private
               | PreType   of loc
               | PreId     of loc * ident
               | PreQId    of loc * ident * ident
               | PreApp    of preterm list
               | PreLam    of loc * ident * preterm * preterm
               | PrePi     of (loc*ident) option * preterm * preterm
               | PreChar   of loc * char
               | PreStr    of loc * string
               | PreNum    of loc * string

val mk_pre_type         : loc -> preterm
val mk_pre_id           : loc -> ident -> preterm
val mk_pre_qid          : loc -> ident -> ident -> preterm
val mk_pre_lam          : loc -> ident -> preterm -> preterm -> preterm
val mk_pre_app          : preterm list -> preterm
val mk_pre_arrow        : preterm -> preterm -> preterm
val mk_pre_pi           : loc -> ident -> preterm -> preterm -> preterm
val mk_pre_char         : loc -> char -> preterm
val mk_pre_string       : loc -> string -> preterm
val mk_pre_num          : loc -> string -> preterm

val get_loc : preterm -> loc

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list

type ptop = loc * ident * prepattern list
type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = pcontext * ptop * preterm

(** {2 Terms/Patterns} *)

type term = private
  | Kind                                 (* Kind *)
  | Type                                 (* Type *)
  | DB     of ident*int                  (* deBruijn *)
  | Const  of ident*ident                (* Global variable *)
  | App    of term list   (* [ f ; a1 ; ... an ] , length >=2 , f not an App *)
  | Lam    of ident*term*term            (* Lambda abstraction *)
  | Pi     of ident option*term*term     (* Pi abstraction *)
  | Meta   of int
  | Char   of char
  | Str    of string
  | Num    of int

val mk_Kind     : term
val mk_Type     : term
val mk_DB       : ident -> int -> term
val mk_Const    : ident -> ident -> term
val mk_Lam      : ident -> term -> term -> term
val mk_App      : term list -> term
val mk_Pi       : ident option -> term -> term -> term
val mk_Unique   : unit -> term
val mk_Meta     : int -> term
val mk_Char     : char -> term
val mk_Str      : string -> term
val mk_Num      : int -> term


val mk_char_type : term
val mk_string_type : term
val mk_num_type : term

val const_env : (ident * term) list
val is_const : ident -> bool
val get_const_ty : ident -> term

(* Syntactic equality / Alpha-equivalence *)
val term_eq : term -> term -> bool

val sugar : term -> term
val unsugar : term -> term

type pattern =
  | Var         of ident*int
  | Brackets    of term
  | Pattern     of ident*ident*pattern array

val term_of_pattern : pattern -> term

type top = ident*pattern array
type context = ( ident * term ) list

(**{2 Rewrite Rules} *)

type rule = {
        l:loc;
        ctx:context;
        id:ident;
        args:pattern array;
        rhs:term; }

type rule2 =
    { loc:loc ; pats:pattern array ; right:term ;
      constraints:(term*term) list ; env_size:int ; }

type dtree =
  | Switch      of int * (int*ident*ident*dtree) list * dtree option
  | Test        of (term*term) list * term * dtree option

type rw_infos =
  | Decl    of term
  | Def     of term*term
  | Decl_rw of term*rule2 list*int*dtree

(** {2 Commands} *)

type command =
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
  | Other of string*preterm list

(** {2 Misc} *)

type yes_no_maybe = Yes | No | Maybe
type 'a option2 = None2 | DontKnow | Some2 of 'a
type ('a,'b) sum = Success of 'a | Failure of 'b
