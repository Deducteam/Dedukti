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
(** [term_eq t t'] is [true] if [t] = [t'] (up to alpha equivalence) *)

type position = int list
(** Position in a term *)

exception InvalidSubterm of term * int

val subterm : term -> position -> term
(** [subterm t p] returns the subterm of [t] at position [p].
    Raises InvalidSubterm in case of invalid position in given term. *)

(** {2 Contexts} *)

type 'a context = (loc * ident * 'a) list
(** Abstract context *)

type part_typed_context = term option context
(** Rule context after they have been parsed *)

type typed_context = term context
(** Rule context after type checking assigns a type to the variables *)

val pp_untyped_context    : 'a context         printer
val pp_typed_context      : typed_context      printer
val pp_part_typed_context : part_typed_context printer


val rename_vars_with_typed_context : typed_context -> term -> term
