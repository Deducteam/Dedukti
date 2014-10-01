open Basics
open Term
open Rule

val ignore_redecl       : bool ref
val autodep             : bool ref

type t

type dtree_or_def =
  | DoD_None
  | DoD_Def of term
  | DoD_Dtree of int*dtree

val dummy               : t
val make                : ident -> t
val get_name            : t -> ident

(** Create the dko file and clear the global context. *)
val export    : t -> unit

(** [Env.get_type l md id] returns the type of the constant symbol [id] in
 the module [md]. *)
val get_type            : t -> loc -> ident -> ident -> term

val get_dtree           : t -> loc -> ident -> ident -> dtree_or_def

(** [Env.add_decl l id ty] declares a constant symbol [id] of type [ty] in the
 the current module. *)
val declare             : t -> loc -> ident -> term -> unit

(** [Env.add_def l id te ty] defines the alias [id] for the term [te] of type
  [ty] in the the current module. *)
val define              : t -> loc -> ident -> term -> term -> unit

(** Add a list of rewrite rules in the context.
All these rules must have the same head symbol and the same arity. *)
val add_rules           : t -> Rule.rule list -> unit

val get_all_rules       : string -> (string*frule list) list
