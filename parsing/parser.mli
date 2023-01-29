(** {2 Parser}

    This module produces [Entry.t] from various kinds of inputs. The
    inputs supported are:

    - Files (see {!val:input_from_file})

    - string (see {!val:input_from_string})

    - stdin (see {!val:input_from_stding})

    Entries are produced as a sequence of entries.
*)

(** Abstract representation of an input. *)
type 'kind input constraint 'kind = [< `File of string | `Stdin | `String]

(** [from_file ~file] returns an [input]. Entries are read from the
    content of the file given by [file]. *)
val from_file : file:string -> [> `File of string] input

(** [from_stdin md] returns [input] from a module name *)
val from_stdin : Kernel.Basic.mident -> [> `Stdin] input

val from_string : Kernel.Basic.mident -> string -> [> `String] input

val md_of_input : 'kind input -> Kernel.Basic.mident

val file_of_input : [`File of string] input -> string

val kind_of_input : 'kind input -> 'kind

type error = {loc : Kernel.Basic.loc; lexbuf : Lexing.lexbuf}

exception Parser_error of error

val to_seq_exn : 'a input -> Entry.entry Seq.t

val to_seq : 'a input -> (Entry.entry, error) result Seq.t
