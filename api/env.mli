(** The main functionalities of Dedukti:
    This is essentialy a wrapper around Signature, Typing and Reduction *)
open Basic
open Term

(** {2 Error Datatype} *)

type env_error =
  | EnvErrorType        of Typing.typing_error
  | EnvErrorSignature   of Signature.signature_error
  | KindLevelDefinition of loc * ident
  | ParseError          of loc * string
  | AssertError         of loc

exception EnvError of env_error


(** {2 The Global Environment} *)

val init        : string -> mident
(** [init name] initializes a new global environement giving it the name of
    the corresponding source file. The function returns the module identifier
    corresponding to this file, built from its basename. Every toplevel
    declaration will be qualified be this name. *)

val get_signature : unit -> Signature.t
(** [get_signature ()] returns the signature used by this module *)

val get_name    : unit -> mident
(** [get_name ()] returns the name of the module. *)

val get_type    : ?loc:loc -> name -> term
(** [get_type l md id] returns the type of the constant [md.id]. *)

val is_static   : loc -> name -> bool
(** [is_static l cst] returns [true] if the symbol is declared as [static], [false] otherwise *)

val get_dtree   : loc -> name -> Dtree.t
(** [get_dtree l md id] returns the decision/matching tree associated with [md.id]. *)

val export      : unit -> unit
(** [export ()] saves the current environment in a [*.dko] file. *)

val import      : loc -> mident -> unit
(** [import lc md] the module [md] in the current environment. *)

val declare : loc -> ident -> Signature.staticity -> term -> unit
(** [declare_constant l id st ty] declares the symbol [id] of type [ty] and
   staticity [st]. *)

val define      : ?loc:loc -> ident -> bool -> term -> term option -> unit
(** [define l id body ty] defined the symbol [id] of type [ty] to be an alias of [body]. *)

val add_rules   : Rule.untyped_rule list -> (Subst.Subst.t * Rule.typed_rule) list
(** [add_rules rule_lst] adds a list of rule to a symbol. All rules must be on the
    same symbol. *)

(** {2 Type checking/inference} *)

val infer : ?ctx:typed_context -> term         -> term

val check : ?ctx:typed_context -> term -> term -> unit

(** {2 Safe Reduction/Conversion} *)
(** terms are typechecked before the reduction/conversion *)

val reduction : ?ctx:typed_context -> ?red:(Reduction.red_cfg) -> term -> term

val are_convertible : ?ctx:typed_context -> term -> term -> bool

val unsafe_reduction : ?red:(Reduction.red_cfg) -> term -> term
