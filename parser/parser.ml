open Basic
open Term

include Internals

exception Parse_error of loc * string

type stream = Lexing.lexbuf

let check_module str md =
  match Menhir_parser.prelude Lexer.token str with
  | None     -> ()
  | Some md' ->
    if not (Basic.mident_eq md md') then
      Basic.debug 0 "[Warning] Module name mismatch: filename \
                     is different from the one of deprecated command #NAME"

let from_channel md ic =
  let str = Lexing.from_channel ic in
  check_module str md;
  Scoping.name := md;
  str

let read str =
  try Menhir_parser.line Lexer.token str with Menhir_parser.Error ->
    let loc = Lexer.get_loc str in
    let msg = Format.sprintf "Unexpected token '%s'." (Lexing.lexeme str) in
    raise (Parse_error(loc, msg))

let handle_channel md f ic =
  let s = from_channel md ic in
  try while true do f (read s) done with End_of_file -> ()

let parse_channel md ic =
  let l = ref [] in
  handle_channel md (fun e -> l := e::!l) ic;
  List.rev !l
