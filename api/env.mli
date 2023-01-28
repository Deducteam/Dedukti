(** An environment is a wrapper around the kernel of Dedukti *)
open Kernel

open Basic
open Term
open Parsers

(** {2 Error Datatype} *)

(** An environment is created from a {!Parser.input}. Environment is the module
    which interacts with the kernel. An environment allows you to change at
    runtime the reduction engine and the printer. The current version of Dedukti
    offers you one reduction engine, but this feature is mainly aim to be used
    with the [dkmeta] tool. The printer of [Env] is different from [Pp] in a
    sense that the module of a constant is not printed if it is the same as the
    current module. *)
type t

(** [dummy ?m ()] returns a dummy environment. If [m] is provided, the
    environment is built from module [m], but without file. *)
val dummy : ?md:mident -> unit -> t

exception Env_error of t * loc * exn

(** {2 Debugging} *)

exception DebugFlagNotRecognized of char

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
val set_debug_mode : string -> unit

(**{2 Utilities} *)

(** Flag to check for variables arity. Default is true. *)
val check_arity : bool ref

(** Flag to check for rules left linearity. Default is false *)
val check_ll : bool ref

(** If [true], a symbol from an external module cannot be imported
   implicitely. The [require] directive must be used. *)
val explicit_import : bool ref

(** {2 The Global Environment} *)

(** [init ~load_path ~input] initializes a new global environement from
   the [input] and [load_path]. [load_path] is used to find object
   files that can be imported during type checking. *)
val init : load_path:Files.t -> input:Parser.input -> t

(** [get_input env] returns the input used to create [env] *)
val get_input : t -> Parser.input

(** [get_input env] returns the filename associated to the input of [env]. We return a fake filename if the input was not create from a filename. *)
val get_filename : t -> string

(** [get_signature env] returns the signature used by this module. *)
val get_signature : t -> Signature.t

(** [get_name env] returns the name of the module. *)
val get_name : t -> mident

(** [get_load_path env] returns the current [load_path] associated to the environment. *)
val get_load_path : t -> Files.t

(** [set_reduction_egine env] changes the reduction engine of [env]. The new environment shares the same signature than [env]. *)
val set_reduction_engine : t -> (module Reduction.S) -> t

(** [get_reduction_engine env] returns the reduction engine of [env] *)
val get_reduction_engine : t -> (module Reduction.S)

(** [get_print env] returns a pretty printer associated to [env] *)
val get_printer : t -> (module Pp.Printer)

module HName : Hashtbl.S with type key = name

(** [get_symbols env] returns the content of the signature [sg]. Each [name] in the current signature is associated to a [rw_infos]. *)
val get_symbols : t -> Signature.rw_infos HName.t

(** [get_type env l md id] returns the type of the constant [md.id]. *)
val get_type : t -> loc -> name -> term

(** [is_injective env l cst] returns [true] if the symbol is declared as [static] or [injective], [false] otherwise *)
val is_injective : t -> loc -> name -> bool

(** [is_static env l cst] returns [true] if the symbol is declared as [static], [false] otherwise *)
val is_static : t -> loc -> name -> bool

(** [get_dtree env l md id] returns the decision/matching tree associated with [md.id]. *)
val get_dtree : t -> loc -> name -> Dtree.t

(** [export env] saves the current environment in a [*.dko] file. *)
val export : t -> unit

(** [import env lc md] the module [md] in the current environment. *)
val import : t -> loc -> mident -> unit

(** [declare_constant env l id st ty] declares the symbol [id] of type [ty] and
    staticity [st]. *)
val declare :
  t -> loc -> ident -> Signature.scope -> Signature.staticity -> term -> unit

(** [define env l id scope body ty] defines the symbol [id] of type [ty] to be an alias of [body]. *)
val define :
  t -> loc -> ident -> Signature.scope -> bool -> term -> term option -> unit

(** [add_rules env rule_lst] adds a list of rule to a symbol. All rules must be on the
    same symbol. *)
val add_rules :
  t ->
  Rule.partially_typed_rule list ->
  (Exsubst.ExSubst.t * Rule.typed_rule) list

(** {2 Type checking/inference} *)

(** [infer env ctx term] infers the type of [term] given the typed context [ctx] *)
val infer : t -> ?ctx:typed_context -> term -> term

(** [infer env ctx te ty] checks that [te] is of type [ty] given the typed context [ctx] *)
val check : t -> ?ctx:typed_context -> term -> term -> unit

(** {2 Safe Reduction/Conversion} *)

(** terms are typechecked before the reduction/conversion *)

(** [reduction env ctx red te] checks first that [te] is well-typed then reduces it
    according to the reduction configuration [red] *)
val reduction :
  t -> ?ctx:typed_context -> ?red:Reduction.red_cfg -> term -> term

(** [are_convertible env ctx tl tr] checks first that [tl] [tr] have the same type,
    and then that they are convertible *)
val are_convertible : t -> ?ctx:typed_context -> term -> term -> bool

(** [unsafe_reduction env red te] reduces [te] according to the reduction configuration [red].
    It is unsafe in the sense that [te] is not type checked first. *)
val unsafe_reduction : t -> ?red:Reduction.red_cfg -> term -> term
