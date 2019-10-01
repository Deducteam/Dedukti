(** An environment is a wrapper around the kernel of Dedukti *)
open Kernel
open Basic
open Term
open Parsers

(** {2 Error Datatype} *)

type t

(** An environment is create from a {!Parser.input}. Environment is the module which interacts with the kernel. An environment allows you to change at runtime the reduction engine and the printer. The current version of Dedukti offers you one reduction engine, but this feature is mainly aim to be used with the [dkmeta] tool. The printer of [Env] is different from [Pp] in a sense that the module of a constant is not printed if it is the same as the current module. *)

exception Env_error of t * loc * exn

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

(**{2 Utilities} *)

val check_arity : bool ref
(** Flag to check for variables arity. Default is true. *)

val check_ll : bool ref
(** Flag to check for rules left linearity. Default is false *)

(** {2 The Global Environment} *)

val init        : Parser.t -> t
(** [init input] initializes a new global environement from the [input] *)

val get_input    : t -> Parser.t
(** [get_input env] returns the input used to create [env] *)

val get_filename : t -> string
(** [get_input env] returns the filename associated to the input of [env]. We return a fake filename if the input was not create from a filename. *)

val get_signature : t -> Signature.t
(** [get_signature env] returns the signature used by this module. *)

val get_name    : t -> mident
(** [get_name env] returns the name of the module. *)

val set_reduction_engine : t -> (module Reduction.S) -> t
(** [set_reduction_egine env] changes the reduction engine of [env]. The new environment shares the same signature than [env]. *)

val get_reduction_engine : t -> (module Reduction.S)
(** [get_reduction_engine env] returns the reduction engine of [env] *)

val get_printer : t -> (module Pp.Printer)
(** [get_print env] returns a pretty printer associated to [env] *)

module HName : Hashtbl.S with type key = name

val get_symbols : t -> Signature.rw_infos HName.t
(** [get_symbols env] returns the content of the signature [sg]. Each [name] in the current signature is associated to a [rw_infos]. *)

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
(** [unsafe_reduction env red te] reduces [te] according to the reduction configuration [red].
    It is unsafe in the sense that [te] is not type checked first. *)

val errors_in_snf : bool ref

val fail_env_error : t -> Basic.loc -> exn -> unit
