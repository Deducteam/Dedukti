(** basic datatypes *)

(** {2 Identifiers (hashconsed strings)} *)
(** Internal representation of identifiers as hashconsed strings. *)

type ident
val empty : ident
val string_of_ident : ident -> string
val pp_ident : out_channel -> ident -> unit
val hstring : string -> ident
val ident_eq : ident -> ident -> bool

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
  | Pi    of loc*ident option*term*term (* Pi abstraction *)
  | Meta  of loc*int
  | Let   of loc*ident*term*term        (* let x=a in b *)

val get_loc : term -> loc

val mk_Kind     : term
val mk_Type     : loc -> term
val mk_DB       : loc -> ident -> int -> term
val mk_Const    : loc -> ident -> ident -> term
val mk_Lam      : loc -> ident -> term -> term -> term
val mk_App      : term -> term -> term list -> term
val mk_Pi       : loc -> ident option -> term -> term -> term
val mk_Unique   : unit -> term
val mk_Meta     : loc -> int -> term
val mk_Let      : loc -> ident -> term -> term -> term

(* Syntactic equality / Alpha-equivalence *)
val term_eq : term -> term -> bool

type pattern =
  | Var         of loc*ident*int
  | Pattern     of loc*ident*ident*pattern list
  | Brackets    of term
  | Joker       of loc*int

type top = ident*pattern array
type context = ( ident * term ) list

(**{2 Rewrite Rules} *)

type rule = {
        l:loc;
        ctx:context;
        md:ident;
        id:ident;
        args:pattern list;
        rhs:term; }

type dtree =
  | Switch      of int * (int*ident*ident*dtree) list * dtree option
  | Test        of (term*term) list * term * dtree option

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
