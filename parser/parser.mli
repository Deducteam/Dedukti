open Basic
open Term
open Entry

(** Abstract parser stream representation. *)
type stream

(** Exception that can be raised by any of the following functions. *)
exception Parse_error of loc * string

val from_channel : mident -> in_channel -> stream
(** [from_channel mod ic] creates a parser [stream] for the module named [mod]
    given the channel [ic]. *)

val read : stream -> entry
(** [read str] reads a single entry from the parser stream [str]. When no more
    [entry] is available, the [End_of_file] exception is raised. *)

val handle_channel : mident -> (entry -> unit) -> in_channel -> unit
(** [handle_channel mod f ic] parses the channel [ic] for module [mod],  using
    the action [f] on each entry. Note that the channel is parsed lazily. This
    function can thus be applied to [stdin]. *)

val parse_channel : mident -> in_channel -> entry list
(** [parse_channel mod ic] completely parses the channel [ic] for module [mod]
    and returns the corresponding list of entries. *)
