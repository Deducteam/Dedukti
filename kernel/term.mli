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
  | Lam   of loc * ident * term * term (** Lambda abstraction *)
  | Pi    of loc * ident * term * term        (** Pi abstraction *)
  | Meta  of loc * ident * int * term option ref


val pp_term : Format.formatter -> term -> unit

val get_loc : term -> loc

val mk_Kind     : term
val mk_Type     : loc -> term
val mk_DB       : loc -> ident -> int -> term
val mk_Const    : loc -> name  -> term
val mk_Lam      : loc -> ident -> term -> term -> term
val mk_App      : term -> term -> term list -> term
val mk_Pi       : loc -> ident -> term -> term -> term
val mk_Arrow    : loc -> term -> term -> term
val mk_Meta     : loc -> ident -> term
val mk_Meta2     : loc -> ident -> int -> term

val new_fresh_meta_id : unit -> int
(** term_eq [t] [t'] is true if [t]=[t'] (up to alpha equivalence) *)
val term_eq : term -> term -> bool

type var = ident
(* TODO: the same as Rule.untyped_context *)
type ctx = ( loc * ident ) list

type mctx = (loc * ident) list

type box_term =
  | MT of loc * ctx * term (* [ g |- term] *)
  | CT of loc * ctx (* [g] *)

type mtype =
  | Impl of loc * mtype * mtype (* mty -> mty' *)
  | Forall of loc * var * box_term * mtype (* forall m : [g|- term], mty *)
  | BoxTy of  box_term (* [g |- term] *)

type mterm =
  | MLamF of loc * ident * box_term * mterm (* x : [g|-term] => mte *)
  | MLamI of loc * ident * mterm * mterm (* e : mt => mte *)
  | BoxTe of box_term (* [g |- term] *)
  | Var of loc * var (* x *)
  | MApp of mterm * mterm (* f x *)
