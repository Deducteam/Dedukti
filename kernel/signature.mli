(** Global Environment *)

open Basic
open Term
open Rule
open Dtree

val ignore_redecl       : bool ref
(** When [ignore_redecl] is [true], allows a constant to be redefined.
    By default, [ignore_redecl] is set to [false].*)

val autodep             : bool ref
(** When [autodep] is [true], handles automically dependencies. Be careful,
    [autodep] makes two hypothesis :
    - Every file declares a module of the same name of the file
    - There is no circular dependencies. *)

type signature_error =
  | FailToCompileModule   of loc * mident
  | UnmarshalBadVersionNumber of loc * string
  | UnmarshalSysError     of loc * string * string
  | UnmarshalUnknown      of loc * string
  | SymbolNotFound        of loc * name
  | AlreadyDefinedSymbol  of loc * ident
  | CannotMakeRuleInfos   of Rule.rule_error
  | CannotBuildDtree      of Dtree.dtree_error
  | CannotAddRewriteRules of loc * ident
  | ConfluenceErrorImport of loc * mident * Confluence.confluence_error
  | ConfluenceErrorRules  of loc * rule_infos list * Confluence.confluence_error
  | ExpectedACUSymbol     of loc * name

exception SignatureError of signature_error

type staticity = Static | Definable of algebra

type t

val make                : mident -> t
(** [make name] creates a new signature withe the name [name]. *)

val get_name            : t -> mident
(** [get_name sg] returns the name of the signature [sg]. *)

val export              : t -> bool
(** [export ()] saves the current environment in a [*.dko] file.*)

val get_id_comparator   : t -> ident_comparator

val get_type            : t -> loc -> name -> term
(** [get_type sg l md id] returns the type of the constant [md.id] inside the environement [sg]. *)

val get_staticity       : t -> loc -> name -> staticity
(** [get_staticity sg l md id] returns the staticity of the symbol [md.id] *)

val get_algebra         : t -> loc -> name -> algebra
(** [get_algebra sg l md id] returns the algebra of the symbol [md.id]. *)

val is_injective        : t -> loc -> name -> bool
(** [is_injective sg l md id] returns true when [md.id] is either static
    or declared as injective. *)

val get_neutral         : t -> loc -> name -> term
(** [get_neutral sg l md id] returns the neutral element of the ACU symbol [md.id]. *)

val is_AC               : t -> loc -> name -> bool
(** [is_AC sg l na] returns true when [na] is declared as AC symbol *)

val import              : t -> loc -> mident -> unit
(** [import sg md] the module [md] in the signature [sg]. *)


val get_dtree           : t -> ?select:(Rule.rule_name -> bool) option -> loc -> name -> dtree option
(** [get_dtree sg pred l md id] returns the decision/matching tree associated with [md.id]
    inside the environment [sg]. *)

val add_declaration     : t -> loc -> ident -> staticity -> term -> unit
(** [add_declaration sg l id st ty] declares the symbol [id] of type [ty]
    and staticity [st] in the environment [sg]. *)

val add_rules           : t -> Rule.typed_rule list -> unit
(** [add_rules sg rule_lst] adds a list of rule to a symbol in the environement [sg].
    All rules must be on the same symbol. *)
