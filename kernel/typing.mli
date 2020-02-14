open Term
open Rule
open Basic

(** Type checking/inference *)

val d_typeChecking : Debug.flag
val d_rule         : Debug.flag

val coc : bool ref

val fail_on_unsatisfiable_constraints : bool ref

type typing_error =
  | KindIsNotTypable
  | ConvertibilityError                of term * typed_context * term * term
  | AnnotConvertibilityError           of loc * ident * typed_context * term * term
  | VariableNotFound                   of loc * ident * int * typed_context
  | SortExpected                       of term * typed_context * term
  | ProductExpected                    of term * typed_context * term
  | InexpectedKind                     of term * typed_context
  | DomainFreeLambda                   of loc
  | CannotInferTypeOfPattern           of pattern * typed_context
  | UnsatisfiableConstraints           of partially_typed_rule * (int * term * term)
  | BracketExprBoundVar                of term * typed_context
  | BracketExpectedTypeBoundVar        of term * typed_context * term
  | BracketExpectedTypeRightVar        of term * typed_context * term
  | TypingCircularity                  of loc * ident * int * typed_context * term
  | FreeVariableDependsOnBoundVariable of loc * ident * int * typed_context * term
  | NotImplementedFeature              of loc
  | Unconvertible                      of loc * term * term
  | Convertible                        of loc * term * term
  | Inhabit                            of loc * term * term

exception Typing_error of typing_error

type typ = term

(** {2 Type Inference/Checking} *)
module type S = sig
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

  val check_rule  : Signature.t -> partially_typed_rule -> Subst.Subst.t * typed_rule
  (** [check_rule sg ru] checks that a rule is well-typed. *)
end

module Make(R:Reduction.S) : S

module Default : S
