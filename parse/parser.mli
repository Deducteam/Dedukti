open Kernel

type stream
(** Abstract parser stream representation. *)

val read : stream -> Entry.entry
(** [read str] reads a single entry from the parser stream [str]. When no more
    [entry] is available, the [End_of_file] exception is raised. *)

module type CHANNEL = sig
  type t

  val lexing_from : t -> Lexing.lexbuf
end

module type S =
sig

  type input

  val from : Basic.mident -> input -> stream
  (** [from_channel mod ic] creates a parser [stream] for the module named
      [mod] given the input [ic]. *)

  val handle : Basic.mident -> (Entry.entry -> unit) -> input -> unit
  (** [handle mod f ic] parses the input [ic] for module [mod],  using
      the action [f] on each entry. Note that the input is parsed lazily. This
      function can thus be applied to [stdin]. *)

  val parse : Basic.mident -> input -> Entry.entry list
  (** [parse mod ic] completely parses the input [ic] for module [mod]
      and returns the corresponding list of entries. *)
end

module Make : functor (C : CHANNEL) -> S with type input = C.t

module Parse_channel : S with type input = in_channel

module Parse_string : S with type input = string
