open Basic

type dep_error =
  | ModuleNotFound of string
  | MultipleModules of string * string list
  | CircularDependencies of string * string list

exception Dep_error of dep_error

type path = string

type data = mident * path

type dep_data =  data * data list

val ignore : bool ref

val make : data -> Entry.entry list -> dep_data

val handle : data -> ((Entry.entry -> unit) -> unit) -> dep_data

val topological_sort : (string * string list) list -> string list
