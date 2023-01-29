(** {1 Files}

    This module aims to be used to associated physical paths to module
    identifiers {!type:Kernel.Basic.mident}. Two kind of physical
    paths can be computed:

    - Usual files  (see {!val:regular_file_extension})

    - Object files (see {!val:object_file_extensin})

*)

(** The load path *)
type load_path

(** A path is just a string *)
type path = string

(** [md path] returns the module identifier associated with the path
    given. This path is the basename (without extension) of the path
    given. *)
val md : path -> Kernel.Basic.mident

(** A file is a string. The type is private so to have guarantees
    about the extension. *)
type file = private File of string [@@unboxed]

(** [empty] represents the empty load path. *)
val empty : load_path

(** [add_path load_path path] adds [path] the [load_path]. *)
val add_path : load_path -> path -> load_path

(** [pp] is a pretty-printer for [t]. *)
val pp : load_path Kernel.Basic.printer

(** Extension of regular files. *)
val regular_file_extension : string

(** Extension of object files. *)
val object_file_extension : string

(** Aims to be used with {!val: file_status_of_module}. *)
type status =
  | File_not_found
  | File_found_once of file
  | File_found_multiple_time of file list

(** [file_status_of_module load_path md] returns [File_not_found] if
   using the [load_path], no file could be found associated with the
   module [md]. Returns [File_found_once file] if a single file
   matches the module [md]. Returns [File_found_multiple_time files]
   if several files match the module [md].

   This function expects that a file associated to a module [md] uses
   the same extension as {!val:regular_file_extension}. *)
val file_status : load_path -> Kernel.Basic.mident -> status

(** [get_status_exn t] is similar to [file_status] except an exception
   is raised if [find_stauts] returned [File_not_found] or
   [File_found_multiple_time].  *)
val get_file_exn : load_path -> Kernel.Basic.loc -> Kernel.Basic.mident -> file

(** [as_object_file ?prefix file] returns the object file associated
   to [file]. Every path of an object file is prefixed with [prefix].
   By default [prefix] is [Filename.current_dir_name]. *)
val as_object_file : ?prefix:path -> file -> file

(** [path_as_file ?(default_path = Filename.current_dir_name) input]
   returns a [file] associated to the [input]. In case of [input] is
   not associated to a physical file, the filename is the
   concatenation of [default_path] with the string representation of
   the module identifier. File extension is given by
   {!val:regular_file_extension}. *)
val input_as_file : [`File of string] Parsers.Parser.input -> file

val find_object_file_exn :
  ?prefix:path -> load_path -> Kernel.Basic.loc -> Kernel.Basic.mident -> string
