open Kernel

type is_opaque    = bool
type is_assertion = bool
type should_fail  = bool


type dep_error =
  | ModuleNotFound of Basic.mident
  | MultipleModules of string * string list
  | CircularDependencies of string * string list
  | NameNotFound of Basic.name
  | NoDep of Basic.mident

type env_error =
  | EnvErrorType        of Typing.typing_error
  | EnvErrorSignature   of Signature.signature_error
  | EnvErrorRule        of Rule.rule_error
  | EnvErrorDep         of dep_error
  | NonLinearRule       of Rule.rule_name
  | NotEnoughArguments  of Basic.ident * int * int * int
  | KindLevelDefinition of Basic.ident
  | ParseError          of string
  | BracketScopingError
  | AssertError

exception EnvError of Basic.mident option * Basic.loc * env_error

(** Possible tests in source files. *)
type test =
  | Convert of Term.term * Term.term
  (** Convertibility between the two given terms. *)
  | HasType of Term.term * Term.term
  (** Typability test, given a term and a type.   *)

(** Single source file entry. *)
type entry =
  | Decl  of Basic.loc * Basic.ident * Signature.staticity * Term.term
  (** Symbol declaration. *)
  | Def   of Basic.loc * Basic.ident * is_opaque * Term.term option * Term.term
  (** Definition (possibly opaque). *)
  | Rules of Basic.loc * Rule.untyped_rule list
  (** Reduction rules declaration. *)
  | Eval  of Basic.loc * Reduction.red_cfg * Term.term
  (** Evaluation command. *)
  | Check of Basic.loc * is_assertion * should_fail * test
  (** Test command. *)
  | Infer of Basic.loc * Reduction.red_cfg * Term.term
  (** Type inference command. *)
  | Print of Basic.loc * string
  (** Printing command. *)
  | DTree of Basic.loc * Basic.mident option * Basic.ident
  (** Decision tree printing. *)
  | Name  of Basic.loc * Basic.mident
  (** Obsolete #NAME command. *)
  | Require  of Basic.loc * Basic.mident
  (** Require command. *)

val pp_entry : entry Basic.printer
