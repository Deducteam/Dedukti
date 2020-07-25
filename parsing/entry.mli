open Kernel
open Basic
open Term

type is_opaque    = bool
type is_assertion = bool
type should_fail  = bool

(** Possible tests in source files. *)
type test =
  | Convert of term * term
  (** Convertibility between the two given terms. *)
  | HasType of term * term
  (** Typability test, given a term and a type.   *)

exception Assert_error of loc

(** Single source file entry. *)
type entry =
  | Decl  of loc * ident * Signature.scope * Signature.staticity * term
  (** Symbol declaration. *)
  | Def   of loc * ident * Signature.scope * is_opaque * term option * term
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
  | Name of loc * mident
  (** @deprecated Ignored #NAME command.
      Module name defaults to the file name without extension. *)
  | Require  of loc * mident
  (** Require command. *)

val loc_of_entry : entry -> loc

val pp_entry : entry printer
