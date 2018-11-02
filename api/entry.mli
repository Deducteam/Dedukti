open Basic
open Term

type is_opaque    = bool
type is_assertion = bool
type should_fail  = bool


(** Possible tests in source files. *)
type test =
  (** Convertibility between the two given terms. *)
  | Convert of term * term
  (** Typability test, given a term and a type.   *)
  | HasType of term * term

(** Single source file entry. *)
type entry =
  (** Symbol declaration. *)
  | Decl  of loc * ident * Signature.scope * Signature.staticity * term
  (** Definition (possibly opaque). *)
  | Def   of loc * ident * is_opaque * term option * term
  (** Reduction rules declaration. *)
  | Rules of loc * Rule.untyped_rule list
  (** Evaluation command. *)
  | Eval  of loc * Reduction.red_cfg * term
  (** Test command. *)
  | Check of loc * is_assertion * should_fail * test
  (** Type inference command. *)
  | Infer of loc * Reduction.red_cfg * term
  (** Printing command. *)
  | Print of loc * string
  (** Decision tree printing. *)
  | DTree of loc * mident option * ident
  (** Obsolete #NAME command. *)
  | Name  of loc * mident
  (** Require command. *)
  | Require  of loc * mident
