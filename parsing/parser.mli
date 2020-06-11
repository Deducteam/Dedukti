open Kernel

type stream
(** Abstract parser stream representation. *)

type input
(** Abstract type for input. *)

exception Parse_error of Basic.loc * string

val input_from_file : string -> input

val input_from_stdin : Basic.mident -> input

val input_from_string : Basic.mident -> string -> input

val md_of_input : input -> Basic.mident

val md_of_file  : string -> Basic.mident

val file_of_input : input -> string option

val close : input -> unit

val read : stream -> Entry.entry
(** [read str] reads a single entry from the parser stream [str]. When no more
    [entry] is available, the [End_of_file] exception is raised. *)

val from : input -> stream
(** [from_channel in] creates a parser [stream] for the environment [env]
    [env] given the input [in]. *)

val handle : input -> (Entry.entry -> unit) -> unit
(** [handle in f] parses the input [in] in the environment [env],  using
    the action [f] on each entry. Note that the input is parsed lazily. This
    function can thus be applied to [stdin]. *)

val parse : input -> Entry.entry list
(** [parse in] completely parses the input [in] and returns the corresponding
    list of entries. *)
