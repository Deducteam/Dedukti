open Kernel
open Parsers

type files_error =
  | ModuleNotFound of Basic.mident
  | MultipleModules of string * string list
  | ObjectFileNotFound of Basic.mident

exception Files_error of files_error

(** [add_path p] add the [p] to the load path *)
val add_path : string -> unit

(** [get_path ()] returns all the paths in the load path *)
val get_path : unit -> string list

(** [get_find_object_file lc md] returns the path assoiated to the module [md]
    or raise an exception *)
val find_object_file : Basic.loc -> Basic.mident -> string

(** [object_file_of_input] returns the filename associated to the input *)
val object_file_of_input : Parser.input -> string

(** [get_file md] returns the path associated to module [md] *)
val get_file : Basic.mident -> string

(** [find_dk md path] looks for the ".dk" file corresponding to the module
    named [name] in the directories of [path]. If no corresponding file is
    found, or if there are several possibilities, the program fails with a
    graceful error message. *)
val find_dk : ignore:bool -> Basic.mident -> string list -> string option
