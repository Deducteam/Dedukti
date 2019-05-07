open Basic

type dep_error =
  | ModuleNotFound of string
  | MultipleModules of string * string list
  | CircularDependencies of string * string list

exception Dep_error of dep_error

type path = string

module MDepSet : Set.S with type elt = Basic.mident * path

module NameSet : Set.S with type elt = Basic.name

type data = {up: NameSet.t ; down: NameSet.t}
(** up dependencies are the name that requires the current item.
    down dependencies are the name that are required by the current item. *)

type ideps = (ident, data) Hashtbl.t

type deps =
  {
    file:path; (** path associated to the module *)
    deps: MDepSet.t; (** pairs of module and its associated path *)
    ideps: ideps; (** up/down item dependencies *)
}

type t = (mident, deps) Hashtbl.t

val deps : t

val ignore : bool ref

val compute_ideps : bool ref

val make : Basic.mident -> path -> Entry.entry list -> unit
(** [make md file es] computes dependencies for the entries [es] in [file] *)

val handle : Basic.mident -> path -> ((Entry.entry -> unit) -> unit) -> unit
(** [handle md file f] computes dependencies on the fly for the entries in [file] *)

val topological_sort : t -> path list
(** [topological_sort f] returns a list of files sorted by their dependencies *)
