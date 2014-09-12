(** Global context management. *)
open Types

val name                : ident ref
val ignore_redecl       : bool ref
val autodep             : bool ref

(** Initialize the global context. *)
val init                : ident -> unit

(** Create the dko file and clear the global context. *)
val export    : unit -> unit
val clear    : unit -> unit

(** [Env.get_infos l md id] returns the context infos corresponding to the
 constant symbol [id] in the module [md]. *)
val get_infos           : loc -> ident -> ident -> rw_infos

(** [Env.get_type l md id] returns the type of the constant symbol [id] in
 the module [md]. *)
val get_type            : loc -> ident -> ident -> term

(** [Env.add_decl l id ty] declares a constant symbol [id] of type [ty] in the
 the current module. *)
val add_decl            : loc -> ident -> term -> unit

(** [Env.add_def l id te ty] defines the alias [id] for the term [te] of type
  [ty] in the the current module. *)
val add_def             : loc -> ident -> term -> term -> unit

(** Add a list of rewrite rules in the context.
All these rules must have the same head symbol and the same arity. *)
val add_rw              : rule list -> unit
