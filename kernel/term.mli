open Basic

(** Lambda terms *)

(** {2 Terms} *)

(* TODO: abstract de Bruijn indices in DB constructor *)
type term = private
  | Kind                                      (** Kind *)
  | Type  of loc                              (** Type *)
  | DB    of loc * ident * int                (** deBruijn indices *)
  | Const of loc * ident * ident              (** Global variable *)
  | App   of term * term * term list          (** f a1 [ a2 ; ... ; an ] , f not an App *)
  | Lam   of loc * ident * term option * term (** Lambda abstraction *)
  | Pi    of loc * ident * term * term        (** Pi abstraction *)

val pp_term : Format.formatter -> term -> unit

val get_loc : term -> loc

val mk_Kind     : term
val mk_Type     : loc -> term
val mk_DB       : loc -> ident -> int -> term
val mk_Const    : loc -> ident -> ident -> term
val mk_Lam      : loc -> ident -> term option -> term -> term
val mk_App      : term -> term -> term list -> term
val mk_Pi       : loc -> ident -> term -> term -> term
val mk_Arrow    : loc -> term -> term -> term

(** term_eq [t] [t'] is true if [t]=[t'] (up to alpha equivalence) *)
val term_eq : term -> term -> bool
