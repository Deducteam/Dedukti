open Basic
open Term

include Internals

exception Parse_error of loc * string

type stream = {mod_name : Basic.mident; lexbuf : Lexing.lexbuf}

let from_channel mod_name ic =
  {mod_name; lexbuf = Lexing.from_channel ic}

let read str =
  try Menhir_parser.line Lexer.token str.lexbuf str.mod_name
  with Menhir_parser.Error ->
    let loc = Lexer.get_loc str.lexbuf in
    let lex = Lexing.lexeme str.lexbuf in
    let msg = Format.sprintf "Unexpected token '%s'." lex in
    raise (Parse_error(loc, msg))

let handle_channel md f ic =
  let s = from_channel md ic in
  try while true do f (read s) done with End_of_file -> ()

let parse_channel md ic =
  let l = ref [] in
  handle_channel md (fun e -> l := e::!l) ic;
  List.rev !l
