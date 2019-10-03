open Basic
open Term


type stream = {mod_name : Basic.mident; lexbuf : Lexing.lexbuf}

exception Parse_error of loc * string

module type S =
sig

  type input

  (** [from_channel mod ic] creates a parser [stream] for the module named
      [mod] given the input [ic]. *)
  val from : mident -> input -> stream

  (** [handle mod f ic] parses the input [ic] for module [mod],  using
      the action [f] on each entry. Note that the input is parsed lazily. This
      function can thus be applied to [stdin]. *)
  val handle : mident -> (Entry.entry -> unit) -> input -> unit

  (** [parse mod ic] completely parses the input [ic] for module [mod]
      and returns the corresponding list of entries. *)
  val parse : mident -> input -> Entry.entry list

end

let read str =
  try Menhir_parser.line Lexer.token str.lexbuf str.mod_name
  with Menhir_parser.Error ->
    let loc = Lexer.get_loc str.lexbuf in
    let lex = Lexing.lexeme str.lexbuf in
    let msg = Format.sprintf "Unexpected token '%s'." lex in
    raise (Parse_error (loc,msg))

module type CHANNEL = sig
  type t

  val lexing_from : t -> Lexing.lexbuf
end

module Make = functor (C : CHANNEL) -> struct

  type input = C.t

  let from mod_name ic =
    {mod_name; lexbuf = C.lexing_from ic}

  let handle md f ic =
    let s = from md ic in
    try
      while true do f (read s) done
    with End_of_file -> ()

  let parse md ic =
    let l = ref [] in
    handle md (fun e -> l := e::!l) ic;
    List.rev !l

end

module Parse_channel =
  Make(struct
      type t = in_channel

      let lexing_from s = Lexing.from_channel s
    end)

module Parse_string =
  Make(struct
      type t = string

      let lexing_from s = Lexing.from_string s
    end)
