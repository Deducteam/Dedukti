type data = {up: Basic.NameSet.t ; down: Basic.NameSet.t}
(** up dependencies are the name that requires the current item.
    down dependencies are the name that are required by the current item. *)

type name_deps = (Basic.ident, data) Hashtbl.t

type file_deps =
  {
    file     : string;            (** path associated to the module *)
    deps     : Basic.MidentSet.t; (** pairs of module and its associated path *)
    name_deps: name_deps;         (** up/down item dependencies. Not computed by default. *)
}

type t = (Basic.mident, file_deps) Hashtbl.t

type dep_error =
  | ModuleNotFound       of Basic.mident
  | MultipleModules      of string * string list
  | CircularDependencies of string * string list
  | NameNotFound         of Basic.name
  | NoDep                of Basic.mident
  | FileNotFound         of Basic.mident

exception Dep_error of dep_error

val deps : t
(** [deps] contains the dependencies computed by the function [handle] *)

val ignore : bool ref
(** (default: [false]) If [true], no exception is raised if a [module] is not in the path *)

val compute_all_deps : bool ref
(** (default: [false]) If [true], compute the fiel [name_deps]. *)

val add_path : string -> unit

val get_path : unit -> string list

val find_object_file : Basic.loc -> Basic.mident -> string

val object_file_of_input : Parser.t -> string

val get_file : Basic.mident -> string
(** [get_file md] returns the path associated to module [md] *)

val get_data : Basic.name -> data
(** [get_data name] returns the data associated to name [name].
    Raise [NameNotfound] if the dependencies for name have not been computed. *)

val make : Basic.mident -> Entry.entry list -> unit
(** [make md es] computes dependencies for the entries [es] in module [md] *)

val handle : Basic.mident -> ((Entry.entry -> unit) -> unit) -> unit
(** [handle md f] computes dependencies on the fly for the entries in module [md] *)

val topological_sort : t -> string list
(** [topological_sort f] returns a list of files sorted by their dependencies *)

val transitive_closure : Basic.name -> unit
(** [transitive_closure n] compute the transitive closure for [n] *)
