open Kernel
open Parse

exception Dep_error of Entry.dep_error

type path = string

module MDepSet : Set.S with type elt = Basic.mident * path

module NameSet : Set.S with type elt = Basic.name

type data = {up: NameSet.t ; down: NameSet.t}
(** up dependencies are the name that requires the current item.
    down dependencies are the name that are required by the current item. *)

type ideps = (Basic.ident, data) Hashtbl.t

type deps =
  {
    file:path; (** path associated to the module *)
    deps: MDepSet.t; (** pairs of module and its associated path *)
    ideps: ideps; (** up/down item dependencies *)
}

type t = (Basic.mident, deps) Hashtbl.t

val deps : t

val ignore : bool ref

val compute_ideps : bool ref
(** Whether to compute dependencies of every element.  If set to
    [false], only module dependencies are computed. *)

val get_file : Basic.mident -> path
(** [get_file md] returns the path associated to module [md] *)

val get_data : Basic.name -> data
(** [get_data name] returns the data associated to name [name].
    Raise [NameNotfound] if the dependencies for name have not been computed. *)

val make : Basic.mident -> Entry.entry list -> unit
(** [make md es] computes dependencies for the entries [es] in module [md] *)

val handle : Basic.mident -> ((Entry.entry -> unit) -> unit) -> unit
(** [handle md f] computes dependencies on the fly for the entries in module [md] *)

val topological_sort : t -> path list
(** [topological_sort f] returns a list of files sorted by their dependencies *)

val transitive_closure : Basic.name -> unit
(** [transitive_closure n] compute the transitive closure for [n] *)
