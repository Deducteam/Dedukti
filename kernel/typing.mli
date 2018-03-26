open Term
open Rule
open Basic

(** Type checking/inference *)

val coc : bool ref

type typing_error =
  | KindIsNotTypable
  | ConvertibilityError of term * typed_context * term * term
  | VariableNotFound of loc * ident * int * typed_context
  | SortExpected of term * typed_context * term
  | ProductExpected of term * typed_context * term
  | InexpectedKind of term * typed_context
  | DomainFreeLambda of loc
  | CannotInferTypeOfPattern of pattern * typed_context
  | CannotSolveConstraints of untyped_rule * (int * term * term) list
  | BracketError1 of term * typed_context
  | BracketError2 of term * typed_context*term
  | FreeVariableDependsOnBoundVariable of loc * ident * int * typed_context * term
  | NotImplementedFeature of loc
  | Unconvertible of loc*term*term
  | Convertible of loc*term*term
  | Inhabit of loc*term*term

exception TypingError of typing_error

type typ = term

(** {2 Type Inference/Checking} *)

val infer       : Signature.t -> typed_context -> term -> typ
(** [infer sg ctx te] infers a type for the term [te] in the signature [sg] and context [ctx]
    The context is assumed to be well-formed *)

val check       : Signature.t -> typed_context -> term -> typ -> unit
(** [check sg ctx te ty] checks that the term [te] has type [ty]
    in the signature [sg] and context [ty.ctx].
    [ty] is assumed to be well-typed in [ctx]
    and [ctx] is assumed to be well-formed *)

val checking    : Signature.t -> term -> term -> unit
(** [checking sg te ty] checks that [te] has type [ty] in the empty context.
    [ty] is typechecked first. *)

val inference   : Signature.t -> term -> typ
(** [inference sg ctx te] infers a type for the term [te] in empty context. *)

val check_rule  : Signature.t -> rule_infos -> unit
(** [check_rule sg ru] checks that a rule is well-typed. *)
