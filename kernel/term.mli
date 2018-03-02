open Basic

(** Lambda terms *)

(** {2 Terms} *)

(* TODO: abstract de Bruijn indices in DB constructor *)
type term = private
  | Kind                                      (** Kind *)
  | Type  of loc                              (** Type *)
  | DB    of loc * ident * int                (** deBruijn indices *)
  | Const of loc * name                       (** Global variable *)
  | App   of term * term * term list          (** f a1 [ a2 ; ... ; an ] , f not an App *)
  | Lam   of loc * ident * term option * term (** Lambda abstraction *)
  | Pi    of loc * ident * term * term        (** Pi abstraction *)

val pp_term : term printer

val get_loc : term -> loc

val mk_Kind     : term
val mk_Type     : loc -> term
val mk_DB       : loc -> ident -> int -> term
val mk_Const    : loc -> name  -> term
val mk_Lam      : loc -> ident -> term option -> term -> term
val mk_App      : term -> term -> term list -> term
val mk_App2     : term -> term list -> term
val mk_Pi       : loc -> ident -> term -> term -> term
val mk_Arrow    : loc -> term -> term -> term

val term_eq : term -> term -> bool
(** term_eq [t] [t'] is true if [t]=[t'] (up to alpha equivalence) *)

type ident_comparator = name -> name -> int
(** Type of ident comparison functions *)

type term_comparator = term -> term -> int
(** Type of term comparison functions *)

val compare_term : ident_comparator -> term_comparator
(** compare_term [id_comp] [t] [t'] compares both terms (up to alpha equivalence).
 * The order relation goes :
 * Kind < Type < Const < DB < App < Lam < Pi
 * Besides
 * Const m v < Const m' v' iif (m,v) < (m',v')
 * DB n < DB n' iif n < n'
 * App f a args < App f' a' args' iif (f,a,args) < (f',a',args')
 * Lam x ty t < Lam x' ty' t'  iif t < t'
 * Pi  x a  b < Pi  x' a'  b'  iif (a,b) < (a',b')
 *)

type algebra = Free | AC | ACU of term
(** Symbols' algebra *)

val is_AC : algebra -> bool
(** Return true iff given algebra is AC or ACU. *)

(** {2 Contexts} *)

(** context of rules after they have been parsed *)
type untyped_context = (loc * ident) list

(** type checking rules implies to give a type to the variables of the context *)
type typed_context = ( loc * ident * term ) list

(** ([n], [t]) represents the term represented by [t]
  * (whichever its representation) under [n] lambda abstractions. *)
type 'a depthed = int * 'a
