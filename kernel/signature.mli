(** Global Environment *)

open Basic
open Term
open Rule
open Dtree

val ignore_redecl       : bool ref
(** When [ignore_redecl] is [true], allows a constant to be redefined.
    By default, [ignore_redecl] is set to [false].*)

type signature_error =
  | FailToCompileModule of loc * Name.mident
  | UnmarshalBadVersionNumber of loc * string
  | UnmarshalSysError of loc * string * string
  | UnmarshalUnknown of loc * string
  | SymbolNotFound of loc * Name.ident
  | AlreadyDefinedSymbol of loc * Name.ident
  | CannotMakeRuleInfos of Rule.rule_error
  | CannotBuildDtree of Dtree.dtree_error
  | CannotAddRewriteRules of loc * Name.ident
  | ConfluenceErrorImport of loc * Name.mident * Confluence.confluence_error
  | ConfluenceErrorRules of loc * rule_infos list * Confluence.confluence_error

exception SignatureError of signature_error

type staticity = Static | Definable

type t

val make                : Name.mident -> t
(** [make name] creates a new signature withe the name [name]. *)

val get_name            : t -> Name.mident
(** [get_name sg] returns the name of the signature [sg]. *)

val export              : t -> bool
(** [export ()] saves the current environment in a [*.dko] file.*)

val get_type            : t -> loc -> Name.ident -> term
(** [get_type sg l md id] returns the type of the constant [md.id] inside the environement [sg]. *)

val get_dtree           : t -> ?select:(Rule.rule_name -> bool) ->
                          loc -> Name.ident -> (int*dtree) option
(** [get_dtree sg pred l md id] returns the decision/matching tree associated with [md.id]
    inside the environment [sg]. *)

val add_declaration     : t -> loc -> Name.ident -> staticity -> term -> unit
(** [add_declaration sg l id st ty] declares the symbol [id] of type [ty]
    and staticity [st] in the environment [sg]. *)

val add_rules           : t -> Rule.typed_rule list -> unit
(** [add_rules sg rule_lst] adds a list of rule to a symbol in the environement [sg].
    All rules must be on the same symbol. *)
