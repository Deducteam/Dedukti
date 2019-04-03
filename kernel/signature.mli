(** Global Environment *)

open Basic
open Term
open Rule

type Debug.flag += D_module

type signature_error =
  | UnmarshalBadVersionNumber of loc * string
  | UnmarshalSysError     of loc * string * string
  | UnmarshalUnknown      of loc * string
  | SymbolNotFound        of loc * name
  | AlreadyDefinedSymbol  of loc * name
  | CannotMakeRuleInfos   of Rule.rule_error
  | CannotBuildDtree      of Dtree.dtree_error
  | CannotAddRewriteRules of loc * ident
  | ConfluenceErrorImport of loc * mident * Confluence.confluence_error
  | ConfluenceErrorRules  of loc * rule_infos list * Confluence.confluence_error
  | GuardNotSatisfied     of loc * term * term
  | CouldNotExportModule  of string

exception SignatureError of signature_error

type staticity = Static | Definable

type t

val make                : string -> t
(** [make file] creates a new signature corresponding to the file [file]. *)

val get_name            : t -> mident
(** [get_name sg] returns the name of the signature [sg]. *)

val export              : t -> unit
(** [export ()] saves the current environment in a [*.dko] file.*)

val import              : t -> loc -> mident -> unit
(** [import sg md] the module [md] in the signature [sg]. *)

val get_md_deps            : loc -> mident -> mident list
(** [get_deps lc md] returns the list of direct dependencies of module [md].
    This function makes the assumption that the file [md.dko] exists. *)

val is_static           : t -> loc -> name -> bool
(** [is_injective sg l cst] is true when [cst] is a static symbol. *)

val get_type            : t -> loc -> name -> term
(** [get_type sg l md id] returns the type of the constant [md.id] inside the
    environement [sg]. *)

val get_dtree           : t -> (Rule.rule_name -> bool) option -> loc -> name -> Dtree.t
(** [get_dtree sg filter l cst] returns the decision/matching tree associated
    with [cst] inside the environment [sg]. When filter is specified, it is used
    to select only the corresponding set of rules  *)

val get_rules           : t -> loc -> name -> rule_infos list
(** [get_rules sg lc cst] returns a list of rules that defines the symbol. *)

val add_external_declaration     : t -> loc -> name -> staticity -> term -> unit
(** [add_external_declaration sg l cst st ty] declares the symbol [id] of type
    [ty] and staticity [st] in the environment [sg]. *)

val add_declaration     : t -> loc -> ident -> staticity -> term -> unit
(** [add_declaration sg l id st ty] declares the symbol [id] of type [ty]
    and staticity [st] in the environment [sg]. *)

val add_rules           : t -> Rule.rule_infos list -> unit
(** [add_rules sg rule_lst] adds a list of rule to a symbol in the environement [sg].
    All rules must be on the same symbol. *)

type symbol_infos =
  {
    name  : name;
    stat  : staticity;
    ty    : term;
    rules : rule_infos list;
  }

val symbols_of : t ->  symbol_infos list
(** [access_signature sg] returns the content of the signature [sg]. *)
