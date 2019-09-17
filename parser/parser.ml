open Basic
open Term


type stream = {md : Basic.mident; lexbuf : Lexing.lexbuf}

module type S =
sig

  type input

  (** [from_channel mod ic] creates a parser [stream] for the module named
      [mod] given the input [ic]. *)
  val from : Env.t -> input -> stream

  (** [handle mod f ic] parses the input [ic] for module [mod],  using
      the action [f] on each entry. Note that the input is parsed lazily. This
      function can thus be applied to [stdin]. *)
  val handle : Env.t -> (Entry.entry -> unit) -> input -> unit

  (** [parse mod ic] completely parses the input [ic] for module [mod]
      and returns the corresponding list of entries. *)
  val parse : Env.t -> input -> Entry.entry list

end

let read str =
  try Menhir_parser.line Lexer.token str.lexbuf str.md
  with Menhir_parser.Error ->
    let loc = Lexer.get_loc str.lexbuf in
    let lex = Lexing.lexeme str.lexbuf in
    let msg = Format.sprintf "Unexpected token '%s'." lex in
    raise (Env.EnvError (None,loc, ParseError msg))

module type CHANNEL = sig
  type t

  val lexing_from : t -> Lexing.lexbuf
end

module Make = functor (C : CHANNEL) -> struct

  type input = C.t

  let from env ic =
    let md = Env.get_name env in
    {md; lexbuf = C.lexing_from ic}

  let handle env f ic =
    let s = from env ic in
    try
      while true do f (read s) done
    with
    | Env.EnvError (None, loc, e) -> raise (Env.EnvError (Some env,loc,e))
    | End_of_file -> ()

  let parse env ic =
    let l = ref [] in
    handle env (fun e -> l := e::!l) ic;
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
