open Basics

(** {2 Terms} *)

type term = private
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | DB    of loc*ident*int              (* deBruijn *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*ident*term option*term        (* Lambda abstraction *)
  | Pi    of loc*ident*term*term                (* Pi abstraction *)

type context = ( loc * ident * term ) list
val pp_context  : out_channel -> context -> unit

val get_loc : term -> loc

val mk_Kind     : term
val mk_Type     : loc -> term
val mk_DB       : loc -> ident -> int -> term
val mk_Const    : loc -> ident -> ident -> term
val mk_Lam      : loc -> ident -> term option -> term -> term
val mk_App      : term -> term -> term list -> term
val mk_Pi       : loc -> ident -> term -> term -> term
val mk_Arrow    : loc -> term -> term -> term

(* Syntactic equality / Alpha-equivalence *)
val term_eq : term -> term -> bool

val pp_term     : out_channel -> term -> unit
