open Term
open Rule
open Basics

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

val infer       : Signature.t -> Context.t -> term -> judgment
val check       : Signature.t -> term -> judgment -> judgment

val inference   : Signature.t -> term -> judgment
val checking    : Signature.t -> term -> term -> judgment
val check_rule  : Signature.t -> rule -> unit

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
