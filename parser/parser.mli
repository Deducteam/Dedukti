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
  | Decl  of loc * ident * Signature.staticity * term
  (** Definition (possibly opaque). *)
  | Def   of loc * ident * is_opaque * term option * term
  (** Reduction rules declaration. *)
  | Rules of Rule.untyped_rule list
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

(** Abstract parser stream representation. *)
type stream

(** Exception that can be raised by any of the following functions. *)
exception Parse_error of loc * string

(** [from_channel mod ic] creates a parser [stream] for the module named [mod]
    given the channel [ic]. If the obsolete "#NAME" construct is given, and if
    it does not match [mod] then a warning is displayed. *)
val from_channel : mident -> in_channel -> stream

(** [read str] reads a single entry from the parser stream [str]. When no more
    [entry] is available, the [End_of_file] exception is raised. *)
val read : stream -> entry

(** [handle_channel mod f ic] parses the channel [ic] for module [mod],  using
    the action [f] on each entry. The obsolete "#NAME" construct is handled as
    described in the documentation of [from_channel]. Note that the channel is
    parsed lazily, and this function can thus be applied to [stdin]. *)
val handle_channel : mident -> (entry -> unit) -> in_channel -> unit

(** [parse_channel mod ic] completely parses the channel [ic] for module [mod]
    and returns the corresponding list of entries. Once again,  if the "#NAME"
    construct is given, it is handled a with [from_channel]. *)
val parse_channel : mident -> in_channel -> entry list
