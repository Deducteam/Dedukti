open Basic
open Term

include Internals

(** Exception that can be raised by any of the following functions. *)
exception Parse_error of loc * string

type stream = Lexing.lexbuf

let check_module lb md =
  match Menhir_parser.prelude Lexer.token lb with
  | None -> ()
  | Some md' ->
    if not (Basic.mident_eq md md') then
      Basic.debug 0 "[Warning] Module name mismatch: filename \
                     is different from the one of deprecated command #NAME"

let from_channel md ic =
  let lexbuf = Lexing.from_channel ic in
  check_module lexbuf md;
  Scoping.name := md;
  lexbuf

let read lb =
  try
    Menhir_parser.line Lexer.token lb
  with
  | Lexer.EndOfFile -> raise End_of_file
  | Menhir_parser.Error -> raise (Parse_error(Lexer.get_loc lb,
                                       Format.sprintf "Unexpected token '%s'." (Lexing.lexeme lb)))
let handle_channel md f ic =
  let s = from_channel md ic in
  try
    while true do f (read s) done
  with
  | End_of_file -> ()


let parse_channel md ic =
  let l = ref [] in
  handle_channel md (fun e -> l := e::!l) ic;
  List.rev !l
