open Kernel

type stream
(** Abstract parser stream representation. *)

type input
(** Abstract type for input. *)

type parser = Legacy | Sukerujo

exception Parse_error of Basic.loc * string

val input_from_file : ?parser:parser -> string -> input

val input_from_stdin : ?parser:parser -> Basic.mident -> input

val input_from_string : ?parser:parser -> Basic.mident -> string -> input

val md_of_input : input -> Basic.mident

val md_of_file  : string -> Basic.mident

val file_of_input : input -> string option

val close : input -> unit

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
