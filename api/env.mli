(** The main functionalities of Dedukti:
    This is essentialy a wrapper around Signature, Typing and Reduction *)
open Basic
open Term

type t

(** {2 Error Datatype} *)

type env_error =
  | KindLevelDefinition of ident (** A definition of type Kind cannot be added in the environment *)
  | Misc                of exn

exception Env_error of t * loc * env_error

(** {2 Debugging} *)

exception DebugFlagNotRecognized of char

val set_debug_mode : string -> unit
(** Sets multiple debugging flags from a string:
      q : disables d_Warn
      n : enables  d_Notice
      o : enables  d_Module
      c : enables  d_Confluence
      u : enables  d_Rule
      t : enables  d_TypeChecking
      r : enables  d_Reduce
      m : enables  d_Matching
    May raise DebugFlagNotRecognized.
*)

val check_arity : bool ref
(** Flag to check for variables arity. Default is true. *)

val check_ll : bool ref
(** Flag to check for rules left linearity. Default is false. *)

(** {2 The Global Environment} *)

val init        : Parser.t -> t
(** [init name] initializes a new global environement giving it the name of
    the corresponding source file. The function returns the module identifier
    corresponding to this file, built from its basename. Every toplevel
    declaration will be qualified by this name. *)

val get_input    : t -> Parser.t

val get_signature : t -> Signature.t
(** [get_signature env] returns the signature used by this module. *)

val get_name    : t -> mident
(** [get_name env] returns the name of the module. *)

val set_reduction_engine : t -> (module Reduction.S) -> t
(** [set_reduction_egine env] changes the reduction engine of [env] *)

val get_reduction_engine : t -> (module Reduction.S)
(** [get_reduction_engine env] returns the reduction engine of [env] *)

val get_printer : t -> (module Pp.Printer)
(** [get_print env] returns a pretty printer associated to [env] *)

module HName : Hashtbl.S with type key = name

val get_symbols : t -> Signature.rw_infos HName.t
(** [get_symbols env] returns the content of the signature [sg]. *)

val get_type    : t -> loc -> name -> term
(** [get_type env l md id] returns the type of the constant [md.id]. *)

val is_static   : t -> loc -> name -> bool
(** [is_static env l cst] returns [true] if the symbol is declared as [static], [false] otherwise *)

val get_dtree   : t -> loc -> name -> Dtree.t
(** [get_dtree env l md id] returns the decision/matching tree associated with [md.id]. *)

val export      : t -> unit
(** [export env] saves the current environment in a [*.dko] file. *)

val import      : t -> loc -> mident -> unit
(** [import env lc md] the module [md] in the current environment. *)

val declare     : t -> loc -> ident -> Signature.staticity -> term -> unit
(** [declare_constant env l id st ty] declares the symbol [id] of type [ty] and
    staticity [st]. *)

val define      : t -> loc -> ident -> bool -> term -> term option -> unit
(** [define env l id body ty] defined the symbol [id] of type [ty] to be an alias of [body]. *)

val add_rules   : t -> Rule.untyped_rule list -> (Subst.Subst.t * Rule.typed_rule) list
(** [add_rules env rule_lst] adds a list of rule to a symbol. All rules must be on the
    same symbol. *)

(** {2 Type checking/inference} *)

val infer : t -> ?ctx:typed_context -> term         -> term
(** [infer env ctx term] infers the type of [term] given the typed context [ctx] *)

val check : t -> ?ctx:typed_context -> term -> term -> unit
(** [infer env ctx te ty] checks that [te] is of type [ty] given the typed context [ctx] *)

(** {2 Safe Reduction/Conversion} *)
(** terms are typechecked before the reduction/conversion *)

val reduction : t -> ?ctx:typed_context -> ?red:(Reduction.red_cfg) -> term -> term
(** [reduction env ctx red te] checks first that [te] is well-typed then reduces it
    according to the reduction configuration [red] *)

val are_convertible : t -> ?ctx:typed_context -> term -> term -> bool
(** [are_convertible env ctx tl tr] checks first that [tl] [tr] have the same type,
    and then that they are convertible *)

val unsafe_reduction : t -> ?red:(Reduction.red_cfg) -> term -> term
(** [unsafe_reduction env red te] reduces [te] according to the reduction configuration [red] *)
