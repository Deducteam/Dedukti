(** Global Environment *)

open Basic
open Term
open Rule

val ignore_redecl       : bool ref
(** When [ignore_redecl] is [true], allows a constant to be redefined.
    By default, [ignore_redecl] is set to [false].*)

val autodep             : bool ref
(** When [autodep] is [true], handles automically dependencies. Be careful,
    [autodep] makes two hypothesis :
    - Every file declares a module of the same name of the file
    - There is no circular dependencies. *)

type signature_error =
  | FailToCompileModule of loc*ident
  | UnmarshalBadVersionNumber of loc*string
  | UnmarshalSysError of loc*string*string
  | UnmarshalUnknown of loc*string
  | SymbolNotFound of loc*ident*ident
  | AlreadyDefinedSymbol of loc*ident
  | CannotBuildDtree of Dtree.dtree_error
  | CannotAddRewriteRules of loc*ident
  | ConfluenceErrorImport of loc*ident*Confluence.confluence_error
  | ConfluenceErrorRules of loc*rule_infos list*Confluence.confluence_error

exception SignatureError of signature_error

type t

val make                : ident -> t
(** [make name] creates a new signature withe the name [name]. *)

val get_name            : t -> ident
(** [get_name sg] returns the name of the signature [sg]. *)

val export              : t -> bool
(** [export ()] saves the current environment in a [*.dko] file.*)

val get_type            : t -> loc -> ident -> ident -> term
(** [get_type sg l md id] returns the type of the constant [md.id] inside the environement [sg]. *)

val is_constant         : t -> loc -> ident -> ident -> bool
(** [is_constant sg l md id] returns true when [mkd.id] is a constant. *)

val get_dtree           : t -> loc -> ident -> ident -> (int*dtree) option
(** [get_dtree sg l md id] returns the decision/matching tree associated with [md.id]
    inside the environment [sg]. *)

val add_declaration     : t -> loc -> ident -> term -> unit
(** [add_declaration sg l id ty] declares the constant symbol [id] of type [ty]
    in the environment [sg]. *)

val add_definable       : t -> loc -> ident -> term -> unit
(** [add_definable sg l id ty] declares the definable symbol [id] of type [ty]
    in the environment [sg]. *)

val add_rules           : t -> Rule.rule2 list -> unit
(** [add_rules sg rule_lst] adds a list of rule to a symbol in the environement [sg].
    All rules must be on the same symbol. *)
