(** The main functionalities of Dedukti:
 this is essentialy a wrapper around Signature, Typing and Reduction *)
open Basic
open Term
open Signature

type env_error =
  | EnvErrorType of Typing.typing_error
  | EnvErrorSignature of signature_error
  | KindLevelDefinition of loc*ident

(** {2 The Global Environment} *)

val init        : mident -> unit
(** [init name] initializes a new global environement giving it the name [name].
    Every top level declaration will be qualified be this name. *)

val get_name    : unit -> mident
(** [get_name ()] returns the name of the module. *)

val get_type    : loc -> name -> (term,signature_error) error
(** [get_type l md id] returns the type of the constant [md.id]. *)

val get_dtree   : loc -> name -> ((int*Dtree.dtree) option,signature_error) error
(** [get_dtree l md id] returns the decision/matching tree associated with [md.id]. *)

val export      : unit -> bool
(** [export ()] saves the current environment in a [*.dko] file. *)

val import      : loc -> mident -> (unit, signature_error) error
(** [import lc md] the module [md] in the current environment. *)

val declare : loc -> ident -> Signature.staticity -> term -> (unit,env_error) error
(** [declare_constant l id st ty] declares the symbol [id] of type [ty] and
   staticity [st]. *)

val define      : loc -> ident -> term -> term option -> (unit,env_error) error
(** [define l id body ty] defined the symbol [id] of type [ty] to be an alias of [body]. *)

val define_op   : loc -> ident -> term -> term option -> (unit,env_error) error
(** [define_op l id body ty] declares the symbol [id] of type [ty] and checks
    that [body] has this type (but forget it after). *)

val add_rules   : Rule.untyped_rule list -> (Rule.typed_rule list,env_error) error
(** [add_rules rule_lst] adds a list of rule to a symbol. All rules must be on the
    same symbol. *)

(** {2 Type checking/inference} *)

val infer : ?ctx:typed_context -> term         -> (term,env_error) error

val check : ?ctx:typed_context -> term -> term -> (unit,env_error) error

(** {2 Safe Reduction/Conversion} *)
(** terms are typechecked before the reduction/conversion *)

val reduction : ?red:(Reduction.red_config) -> term -> (term,env_error) error

val are_convertible : term -> term -> (bool,env_error) error

val unsafe_reduction : ?red:(Reduction.red_config) -> term -> term
