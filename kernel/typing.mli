open Term
open Rule
open Basics

(** Type checking/inference *)

val coc : bool ref

type typing_error =
  | KindIsNotTypable
  | ConvertibilityError of term*context*term*term
  | VariableNotFound of loc*ident*int*context
  | SortExpected of term*context*term
  | ProductExpected of term*context*term
  | InexpectedKind of term*context
  | DomainFreeLambda of loc

exception TypingError of typing_error

type 'a judgment0 = private { ctx:'a; te:term; ty: term; }

module Context :
sig
  type t
  val empty : t
  val add : loc -> ident -> t judgment0 -> t
  val get_type : t -> loc -> ident -> int -> term
  val is_empty : t -> bool
end

type judgment = Context.t judgment0

(** {2 Type Inference/Checking} *)

val infer       : Signature.t -> Context.t -> term -> judgment
(** [infer sg ctx te] builds a typing judgment for the term [te] in the signature [sg] and context [ctx] *)

val check       : Signature.t -> term -> judgment -> judgment
(** [check sg te ty] builds a typing judgment for the term [te] of type [ty.te]
* in the signature [sg] and context [ty.ctx]. *)

val checking    : Signature.t -> term -> term -> judgment
(** [checking sg te ty] checks, in the empty context, that [ty] is the type of
  * [te]. [ty] is typechecked first. *)

val inference   : Signature.t -> term -> judgment
(** [inference sg ctx te] builds a typing judgment for the term [te] in the signature [sg] and empty context. *)

val check_rule  : Signature.t -> rule -> unit
(** [check_rule sg ru] checks that a rule is well-typed. *)

(** {2 Judgment Constructors (experimental)} *)

type judgmentExn =
  | DistinctContexts
  | LambdaKind
  | LambdaEmptyContext
  | PiSort
  | PiEmptyContext
  | AppNotAPi
  | AppNotConvertible
  | ConvSort
  | ConvError

exception JudgmentExn of judgmentExn

val mk_Type     : Context.t -> loc -> judgment
val mk_Const    : Signature.t -> Context.t -> loc -> ident -> ident -> judgment
val mk_Var      : Context.t -> loc -> ident -> int -> judgment
val mk_App      : Signature.t -> judgment -> judgment -> judgment
val mk_Pi       : judgment -> judgment
val mk_Lam      : judgment -> judgment
val mk_Conv     : Signature.t -> judgment -> judgment -> judgment
