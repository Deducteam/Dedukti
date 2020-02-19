(** Global Environment *)

open Basic
open Term
open Rule

val d_module : Debug.flag

type file = string

type signature_error =
  | UnmarshalBadVersionNumber of loc * file
  | UnmarshalSysError     of loc * file * string
  | UnmarshalUnknown      of loc * file
  | SymbolNotFound        of loc * name
  | AlreadyDefinedSymbol  of loc * name
  | CannotMakeRuleInfos   of Rule.rule_error
  | CannotBuildDtree      of Dtree.dtree_error
  | CannotAddRewriteRules of loc * name
  | ConfluenceErrorImport of loc * mident * Confluence.confluence_error
  | ConfluenceErrorRules  of loc * rule_infos list * Confluence.confluence_error
  | GuardNotSatisfied     of loc * term * term
  | CannotExportModule    of mident * exn
  | PrivateSymbol         of loc * name

exception Signature_error of signature_error
(** Wrapper exception for errors occuring while handling a signature. *)

type staticity = Static | Definable | Injective
(** Is the symbol allowed to have rewrite rules or not ?
    And if it has, can it be considered injective by the type-checker ? *)
(*  FIXME With the current implementation, one is not allowed to write
    [injective f := t], making the syntax not homogeneous.
    However, it would be useless to declare such a definition as injective,
    since a defined symbol cannot occur at the head of a WHNF,
    hence, the conversion test will never wonder if the convertibility between
    the arguments of [f] implies the convertibility of the whole term. *)

type scope = Public | Private
(** Should the symbol be accessible from outside its definition file ? *)

type t
(** A collection of well-typed symbols and rewrite rules. *)

val make                : mident -> (loc -> mident -> file) -> t
(** [make file] creates a new signature corresponding to the file [file]. *)

val get_name            : t -> mident
(** [get_name sg] returns the name of the signature [sg]. *)

val export              : t -> out_channel -> unit
(** [export sg oc] saves the current environment in [oc] file.*)

val import              : t -> loc -> mident -> unit
(** [import sg md] impots the module [md] in the signature [sg]. *)

val import_signature    : t -> t -> unit
(** [import sg sg_ext] imports the signature [sg_ext] into the signature [sg]. *)

val is_static           : t -> loc -> name -> bool
(** [is_static sg l cst] is true when [cst] is a static symbol. *)

val is_injective        : t -> loc -> name -> bool
(** [is_injective sg l cst] is true when [cst] is an injective symbol. *)

val get_type            : t -> loc -> name -> term
(** [get_type sg l md id] returns the type of the constant [md.id] inside the
    environement [sg]. *)

val get_dtree           : t -> loc -> name -> Dtree.t
(** [get_dtree sg filter l cst] returns the decision/matching tree associated
    with [cst] inside the environment [sg]. *)

val get_rules           : t -> loc -> name -> rule_infos list
(** [get_rules sg lc cst] returns a list of rules that defines the symbol. *)

val add_external_declaration     : t -> loc -> name -> scope -> staticity -> term -> unit
(** [add_external_declaration sg l cst sc st ty] declares the symbol [id] of type
    [ty], scope [sc] and staticity [st] in the environment [sg]. *)

val add_declaration     : t -> loc -> ident -> scope -> staticity -> term -> unit
(** [add_declaration sg l id sc st ty] declares the symbol [id] of type [ty]
    and staticity [st] in the environment [sg].
    If [sc] is [Private] then the symbol cannot be used in other modules *)

val add_rules           : t -> Rule.rule_infos list -> unit
(** [add_rules sg rule_lst] adds a list of rule to a symbol in the environement [sg].
    All rules must be on the same symbol. *)

val fail_on_symbol_not_found : bool ref
(** if [false], does 2 things:
    1. [get_dtree] won't fail if the symbol has not be found
    2. [add_rules] won't fail if a rule is added on a symbol not present
       in the signature. However, a fresh symbol is added.
    This flag is intented to facilitate the use of the module Reduction
    when it is used without the module Typing such as in dkmeta. *)

type rw_infos =
  {
    stat          : staticity;
    (** Whether a symbol is definable *)
    ty            : term;
    (** The type of a symbol *)
    scope         : scope;
    (** The scope of the symbol ([Public]/[Private]) *)
    rules         : rule_infos list;
    (** The list of rules associated to a symbol.
        They are ordored by their declaration within a file and in order they are imported
        in the signature *)
    decision_tree : Dtree.t option
    (** The decision tree computed for the set of rules declared above *)
  }

val fold_symbols : (mident -> ident -> rw_infos -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_symbols f sg t] folds the function [f] on all symbol_infos in the signature
    starting from [t]. *)

val iter_symbols : (mident -> ident -> rw_infos -> unit) -> t -> unit
(** [iter_symbols f sg] iters the function [f] on all symbol_infos in the signature. *)
