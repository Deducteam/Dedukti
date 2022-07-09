(** Global Environment *)

open Basic
open Term
open Rule

val d_module : Debug.flag

type file = string

type signature_error =
  | UnmarshalBadVersionNumber of loc * file
  | UnmarshalSysError of loc * file * string
  | UnmarshalUnknown of loc * file
  | SymbolNotFound of loc * name
  | AlreadyDefinedSymbol of loc * name
  | CannotMakeRuleInfos of Rule.rule_error
  | CannotBuildDtree of Dtree.dtree_error
  | CannotAddRewriteRules of loc * name
  | ConfluenceErrorImport of loc * mident * Confluence.confluence_error
  | ConfluenceErrorRules of loc * rule_infos list * Confluence.confluence_error
  | GuardNotSatisfied of loc * term * term
  | CannotExportModule of mident * exn
  | PrivateSymbol of loc * name
  | ExpectedACUSymbol of loc * name

(** Wrapper exception for errors occuring while handling a signature. *)
exception Signature_error of signature_error

type staticity =
  | Static
  | Definable of algebra
  | Injective
      (** Is the symbol allowed to have rewrite rules or not ?
    And if it has, can it be considered injective by the type-checker ? *)

(* FIXME With the current implementation, one is not allowed to write
   [injective f := t], making the syntax not homogeneous.
   However, it would be useless to declare such a definition as injective,
   since a defined symbol cannot occur at the head of a WHNF,
   hence, the conversion test will never wonder if the convertibility between
   the arguments of [f] implies the convertibility of the whole term. *)

type scope =
  | Public
  | Private
      (** Should the symbol be accessible from outside its definition file ? *)

(** A collection of well-typed symbols and rewrite rules. *)
type t

(** [make name] creates a new signature under the name [name]. *)
val make : mident -> t

(** [get_name sg] returns the name of the signature [sg]. *)
val get_name : t -> mident

(** [export sg oc] saves the current environment in [oc] file.*)
val export : t -> out_channel -> unit

val get_id_comparator : t -> name comparator

(** [import sg sg_ext] imports the signature [sg_ext] into the signature [sg]. *)
val import_signature : t -> t -> unit

(** [is_static sg l cst] is true when [cst] is a static symbol. *)
val is_static : t -> loc -> name -> bool

(** [is_injective sg l cst] is true when [cst] is either static
    or declared as injective. *)
val is_injective : t -> loc -> name -> bool

(** [get_type sg l md id] returns the type of the constant [md.id] inside the
    environement [sg]. *)
val get_type : t -> loc -> name -> term

(** [get_staticity sg l md id] returns the staticity of the symbol [md.id] *)
val get_staticity : t -> loc -> name -> staticity

(** [get_algebra sg l md id] returns the algebra of the symbol [md.id]. *)
val get_algebra : t -> loc -> name -> algebra

(** [get_neutral sg l md id] returns the neutral element of the ACU symbol [md.id]. *)
val get_neutral : t -> loc -> name -> term

(** [is_AC sg l na] returns true when [na] is declared as AC symbol *)
val is_AC : t -> loc -> name -> bool

(** [import sg md filename] the module [filename] as [md] in the signature [sg]. *)
val import : t -> loc -> mident -> string -> mident list

val mem : t -> mident -> bool

(** [get_dtree sg filter l cst] returns the decision/matching tree associated
    with [cst] inside the environment [sg]. *)
val get_dtree : t -> loc -> name -> Dtree.t

(** [get_rules sg lc cst] returns a list of rules that defines the symbol. *)
val get_rules : t -> loc -> name -> rule_infos list

(** [add_external_declaration sg l cst sc st ty] declares the symbol [id] of type
    [ty], scope [sc] and staticity [st] in the environment [sg]. *)
val add_external_declaration :
  t -> loc -> name -> scope -> staticity -> term -> unit

(** [add_declaration sg l id sc st ty] declares the symbol [id] of type [ty]
    and staticity [st] in the environment [sg].
    If [sc] is [Private] then the symbol cannot be used in other modules *)
val add_declaration : t -> loc -> ident -> scope -> staticity -> term -> unit

(** [add_rules sg rule_lst] adds a list of rule to a symbol in the environement [sg].
    All rules must be on the same symbol. *)
val add_rules : t -> Rule.rule_infos list -> unit

type rw_infos = {
  stat : staticity;  (** Whether a symbol is definable *)
  ty : term;  (** The type of a symbol *)
  scope : scope;  (** The scope of the symbol ([Public]/[Private]) *)
  rules : rule_infos list;
      (** The stack pile of rules associated to a symbol.
        They are imported in the signature in the order by they are declared
        within the file *)
  decision_tree : Dtree.t option;
      (** The decision tree computed for the set of rules declared above *)
}

val get_rw_infos : t -> mident -> ident -> rw_infos option

(** [fold_symbols f sg t] folds the function [f] on all symbol_infos in the signature
    starting from [t]. *)
val fold_symbols : (mident -> ident -> rw_infos -> 'a -> 'a) -> t -> 'a -> 'a

(** [iter_symbols f sg] iters the function [f] on all symbol_infos in the signature. *)
val iter_symbols : (mident -> ident -> rw_infos -> unit) -> t -> unit
