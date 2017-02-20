open Term
open Rule
open Basic

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
  | CannotInferTypeOfPattern of pattern*context
  | CannotSolveConstraints of rule * (int*term*term) list
  | BracketError1 of term*context
  | BracketError2 of term*context*term
  | FreeVariableDependsOnBoundVariable of loc*ident*int*context*term
  | NotImplementedFeature of loc

exception TypingError of typing_error

type typ = term
type context = (loc*ident*term) list

(** {2 Type Inference/Checking} *)

val infer       : Signature.t -> context -> term -> typ
(** [infer sg ctx te] infers a type for the term [te] in the signature [sg] and context [ctx]
    The context is assumed to be well-formed *)

val check       : Signature.t -> context -> term -> typ -> unit
(** [check sg ctx te ty] checks that the term [te] has type [ty]
    in the signature [sg] and context [ty.ctx].
    [ty] is assumed to be well-typed in [ctx]
    and [ctx] is assumed to be well-formed *)

val checking    : Signature.t -> term -> term -> unit
(** [checking sg te ty] checks that [te] has type [te] in the empty context.
    [ty] is typechecked first. *)

val inference   : Signature.t -> term -> typ
(** [inference sg ctx te] infers a type for the term [te] in empty context. *)

val check_rule  : Signature.t -> rule -> rule2
(** [check_rule sg ru] checks that a rule is well-typed. *)
