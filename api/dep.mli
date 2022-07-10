(** Module which handle dependencies between Dedukti files *)
open Kernel

open Parsers

(** {2 Debugging} *)

type dep_error =
  | CircularDependencies of string * string list
  | NameNotFound of Basic.name

exception Dep_error of dep_error

(** {2 Type declaration} *)

(** up dependencies are the name that requires the current item.
    down dependencies are the name that are required by the current item. *)
type data = {up : Basic.NameSet.t; down : Basic.NameSet.t}

(** A map from an identifiers to its up and down dependencies *)
type name_deps = (Basic.ident, data) Hashtbl.t

type file_deps = {
  file : string;  (** path associated to the module *)
  deps : Basic.MidentSet.t;  (** pairs of module and its associated path *)
  name_deps : name_deps;
      (** up/down item dependencies. Not computed by default. *)
}

(** Map to a module a file dependencies which contains all the dependencies *)
type t = (Basic.mident, file_deps) Hashtbl.t

(** {2 Dependencies function} *)

(** [deps] contains the dependencies computed by the function [handle] *)
val deps : t

(** (default: [false]) If [true], no exception is raised if a [module] is not in the path *)
val ignore : bool ref

(** Whether to compute the dependencies of constants.  If set to
   [false], only module dependencies are computed. *)
val compute_all_deps : bool ref

(** [get_data name] returns the data associated to name [name].
    Raise [NameNotfound] if the dependencies for name have not been computed. *)
val get_data : Basic.name -> data

(** [make md es] computes dependencies for the entries [es] in module [md] *)
val make : ?filename:string -> Basic.mident -> Entry.entry list -> unit

(** [handle md f] computes dependencies on the fly for the entries in module [md] *)
val handle : Basic.mident -> ((Entry.entry -> unit) -> unit) -> unit

(** [topological_sort f] returns a list of files sorted by their dependencies *)
val topological_sort : t -> string list

(** [transitive_closure n] compute the transitive closure for [n] *)
val transitive_closure : Basic.name -> unit
