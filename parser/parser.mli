open Basic
open Term
open Entry

(** Abstract parser stream representation. *)
type stream
       
(** [read str] reads a single entry from the parser stream [str]. When no more
    [entry] is available, the [End_of_file] exception is raised. *)
val read : stream -> entry

module type To_parse = sig
  type t

  val lexing_from : t -> Lexing.lexbuf
end

module Parser :
  functor (Tp : To_parse) -> sig

    (** [from_channel mod ic] creates a parser [stream] for the module named 
        [mod] given the input [ic]. *)
  val from : mident -> Tp.t -> stream

  (** [handle mod f ic] parses the input [ic] for module [mod],  using
    the action [f] on each entry. Note that the input is parsed lazily. This
    function can thus be applied to [stdin]. *)
  val handle : mident -> (entry -> unit) -> Tp.t -> unit

  (** [parse mod ic] completely parses the input [ic] for module [mod]
    and returns the corresponding list of entries. *)
  val parse : mident -> Tp.t -> entry list

end

module Parse_channel : sig
  val from : mident -> in_channel -> stream
  val handle : mident -> (entry -> unit) -> in_channel -> unit
  val parse : mident -> in_channel -> entry list
end

module Parse_string : sig
  val from : mident -> string -> stream
  val handle : mident -> (entry -> unit) -> string -> unit
  val parse : mident -> string -> entry list
end
