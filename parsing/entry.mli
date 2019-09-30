open Kernel
open Basic
open Term

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
  | NotEnoughArguments  of ident * int * int * int
  | KindLevelDefinition of ident
  | ParseError          of string
  | BracketScopingError
  | AssertError

exception EnvError of mident option * loc * env_error


(** Possible tests in source files. *)
type test =
  | Convert of term * term
  (** Convertibility between the two given terms. *)
  | HasType of term * term
  (** Typability test, given a term and a type.   *)

(** Single source file entry. *)
type entry =
  | Decl  of loc * ident * Signature.staticity * term
  (** Symbol declaration. *)
  | Def   of loc * ident * is_opaque * term option * term
  (** Definition (possibly opaque). *)
  | Rules of loc * Rule.untyped_rule list
  (** Reduction rules declaration. *)
  | Eval  of loc * Reduction.red_cfg * term
  (** Evaluation command. *)
  | Check of loc * is_assertion * should_fail * test
  (** Test command. *)
  | Infer of loc * Reduction.red_cfg * term
  (** Type inference command. *)
  | Print of loc * string
  (** Printing command. *)
  | DTree of loc * mident option * ident
  (** Decision tree printing. *)
  | Name  of loc * mident
  (** Obsolete #NAME command. *)
  | Require  of loc * mident
  (** Require command. *)

val loc_of_entry : entry -> loc

val pp_entry : entry printer
