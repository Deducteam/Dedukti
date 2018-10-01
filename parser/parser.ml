open Basic
open Term

type stream = {mod_name : Basic.mident; lexbuf : Lexing.lexbuf}

let read str =
  try Menhir_parser.line Lexer.token str.lexbuf str.mod_name
  with Menhir_parser.Error ->
    let loc = Lexer.get_loc str.lexbuf in
    let lex = Lexing.lexeme str.lexbuf in
    let msg = Format.sprintf "Unexpected token '%s'." lex in
    raise (Env.EnvError (loc, Env.ParseError msg))
            
module type To_parse = sig
  type t

  val lexing_from : t -> Lexing.lexbuf
end
                
module Parser = functor (Tp : To_parse) -> struct
  let from mod_name ic =
    {mod_name; lexbuf = Tp.lexing_from ic}

  let handle md f ic =
    let s = from md ic in
    try while true do f (read s) done with End_of_file -> ()

  let parse md ic =
    let l = ref [] in
    handle md (fun e -> l := e::!l) ic;
    List.rev !l

end

module Parse_channel =
  Parser(struct
      type t = in_channel

      let lexing_from = Lexing.from_channel
    end)

module Parse_string =
  Parser(struct
      type t = string

      let lexing_from = Lexing.from_string
    end)
