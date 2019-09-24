open Basic

exception Parse_error of loc * string

(** Abstract parser stream representation. *)
type stream

type t

val input_from_file : string -> t

val input_from_stdin : Basic.mident -> t

val input_from_string : Basic.mident -> string -> t

val md_of_input : t -> Basic.mident

val file_of_input : t -> string option

val close : t -> unit

(** [read str] reads a single entry from the parser stream [str]. When no more
    [entry] is available, the [End_of_file] exception is raised. *)
val read : stream -> Entry.entry

(** [from_channel in] creates a parser [stream] for the environment [env]
    [env] given the input [in]. *)
val from : t -> stream

(** [handle in f] parses the input [in] in the environment [env],  using
    the action [f] on each entry. Note that the input is parsed lazily. This
    function can thus be applied to [stdin]. *)
val handle : t -> (Entry.entry -> unit) -> unit

(** [parse [in] env] completely parses the input [in] for the environment [env]
    and returns the corresponding list of entries. *)
val parse : t -> Entry.entry list
