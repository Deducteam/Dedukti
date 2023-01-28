(** {2 Parser}

    This module produces [Entry.t] from various kinds of inputs. The
    inputs supported are:

    - Files (see {!val:input_from_file})

    - string (see {!val:input_from_string})

    - stdin (see {!val:input_from_stding})

    Entries are produced as a sequence of entries.
*)

(** Abstract representation of an input. *)
type input

(** [from_file ~file] returns an [input]. Entries are read from the
    content of the file given by [file]. *)
val from_file : file:string -> input

(** [from_stdin md] returns [input] from a module name *)
val from_stdin : Kernel.Basic.mident -> input

val from_string : Kernel.Basic.mident -> string -> input

val md_of_input : input -> Kernel.Basic.mident

val file_of_input : input -> string option

type error = {loc : Kernel.Basic.loc; lexbuf : Lexing.lexbuf}

exception Parser_error of error

val to_seq_exn : input -> Entry.entry Seq.t

val to_seq : input -> (Entry.entry, error) result Seq.t

val close : input -> unit
