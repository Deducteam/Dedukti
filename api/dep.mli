open Basic

type dep_error =
  | ModuleNotFound of string
  | MultipleModules of string * string list
  | CircularDependencies of string * string list

exception Dep_error of dep_error

type path = string

type data = mident * path

type mdep_data =  data * data list

val ignore : bool ref

val make : data -> Entry.entry list -> mdep_data
(** [make (md,file) es] computes dependencies for the entries [es] in [file] *)


val handle : data -> ((Entry.entry -> unit) -> unit) -> mdep_data
(** [handle (md,file) f] computes dependencies on the fly for the entries in [file] *)

val topological_sort : mdep_data list -> string list
(** [topological_sort f] returns a list of files sorted by their dependencies *)
