(** basic datatypes *)

(** {2 Identifiers (hashconsed strings)} *)
(** Internal representation of identifiers as hashconsed strings. *)

type ident
val string_of_ident : ident -> string
val pp_ident : out_channel -> ident -> unit
val hstring : string -> ident
val ident_eq : ident -> ident -> bool
val qmark : ident

(** {2 Lists with Length} *)

module LList : sig
  type +'a t = private {
    len : int;
    lst : 'a list;
  }

  val cons : 'a -> 'a t -> 'a t
  val nil : 'a t
  val is_empty : _ t -> bool
  val len : _ t -> int
  val lst : 'a t -> 'a list

  val make : len:int -> 'a list -> 'a t
  val make_unsafe : len:int -> 'a list -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val append_l : 'a t -> 'a list -> 'a t
  val nth : 'a t -> int -> 'a
  val remove : int -> 'a t -> 'a t
end

(** {2 Localization} *)

type loc
val dloc                : loc
val mk_loc              : int -> int -> loc
(** mk_loc [line] [column] *)
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
  | STRING      of string

exception EndOfFile

(** {2 PreTerms/PrePatterns} *)

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm * preterm
  | PrePi   of loc * ident option * preterm * preterm

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list
  | PLambda     of loc*ident*prepattern
  | PJoker      of loc

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = loc * pdecl list * ident * prepattern list * preterm

(** {2 Terms/Patterns} *)

type term = private
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | DB    of loc*ident*int              (* deBruijn *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*ident*term*term        (* Lambda abstraction *)
  | Pi    of loc*ident*term*term (* Pi abstraction *)

val get_loc : term -> loc

val mk_Kind     : term
val mk_Type     : loc -> term
val mk_DB       : loc -> ident -> int -> term
val mk_Const    : loc -> ident -> ident -> term
val mk_Lam      : loc -> ident -> term -> term -> term
val mk_App      : term -> term -> term list -> term
val mk_Pi       : loc -> ident -> term -> term -> term
val mk_Arrow    : loc -> term -> term -> term

(* Syntactic equality / Alpha-equivalence *)
val term_eq : term -> term -> bool

type pattern =
  | MatchingVar of loc*ident*int*(loc*ident*int) list
  | BoundVar    of loc*ident*int*pattern list
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term
  | Joker       of loc

val get_loc_pat : pattern -> loc

type top = ident*pattern array
type context = ( ident * term ) list

(**{2 Rewrite Rules} *)

type rule = {
  l:loc; ctx:context; md:ident; id:ident; args:pattern list; rhs:term; }

type case =
  | CConst of int*ident*ident
  | CDB    of int*int
  | CLam

(* Abstract (from a stack (or a term list)) matching problem *)
type abstract_pb = int (*c*) * int list (*(k_i)_{i<=n}*)
(* It corresponds to the following matching problem (modulo beta):
 * stck.(c) ~? F( (DB k_0) ... (DB k_n) )
 * where F is the variable
 * *)

(* Infos to build the context from the stack *)
type pre_context =
  | Syntactic of int LList.t
  (* the list of positions in the stack corresponding to the context. *)
  | MillerPattern of abstract_pb LList.t
  (* the list of abstract problem which list of solutions gives the context. *)

type dtree =
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of pre_context * (term*term) list * term * dtree option

(** {2 Environment} *)

module H : Hashtbl.S with type key := ident

type rw_infos =
  | Decl    of term
  | Def     of term*term
  | Decl_rw of term*rule list*int*dtree

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

(** {2 Util} *)

val bind_opt : ('a -> 'b option) -> 'a option -> 'b option
val map_opt : ('a -> 'b) -> 'a option -> 'b option
