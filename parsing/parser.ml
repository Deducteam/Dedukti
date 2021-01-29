open Kernel
open Basic

type stream = {md : Basic.mident; lexbuf : Lexing.lexbuf}

type from =
  | Channel of in_channel
  | String of string (* String of Dedukti code *)

type parser = Legacy | Sukerujo

type input =
  {
    file: string option;
    md: Basic.mident;
    from:from;
    parser:parser;
  }

let lexing_from input =
  match input with
  | String s -> Lexing.from_string s
  | Channel ic -> Lexing.from_channel ic

let from input =
  let md = input.md in
  {md; lexbuf = lexing_from input.from}

exception Parse_error of loc * string

module type Parser =
sig
  type entry

  val handle : entry -> (Entry.entry -> unit) -> unit

  val read : stream -> entry
end

module Legacy =
struct
  type entry = Entry.entry

  let handle e f = f e

  let read str =
    try
      Menhir_parser.line Lexer.token str.lexbuf str.md
    with Menhir_parser.Error ->
      let loc = Lexer.get_loc str.lexbuf in
      let lex = Lexing.lexeme str.lexbuf in
      let msg = Format.sprintf "Unexpected token '%s'." lex in
      raise  @@ Parse_error(loc,msg)
end

module Sukerujo =
struct
  type entry = Entry.entry list

  let handle e f = List.iter f e

  let read str =
    try
      Sukerujo_parser.line Sukerujo_lexer.token str.lexbuf str.md
    with Sukerujo_parser.Error ->
      let loc = Sukerujo_lexer.get_loc str.lexbuf in
      let lex = Lexing.lexeme str.lexbuf in
      let msg = Format.sprintf "Unexpected token '%s'." lex in
      raise  @@ Parse_error(loc,msg)
end

let md_of_file file =
  let open Filename in
  let base = basename file in
  let base = if check_suffix base ".dk" then (chop_suffix base ".dk") else base in
  mk_mident base

let input_from_file ?(parser=Legacy) file =
  let md = md_of_file file in
  let from = Channel (open_in file) in
  {file=Some file;from;md; parser}

let input_from_stdin ?(parser=Legacy) md = {file=None;from=Channel stdin;md; parser}

let input_from_string ?(parser=Legacy) md s = {file=None; from=String s;md; parser}

let md_of_input t = t.md

let file_of_input t = t.file

let close input =
  match input.from with
  | String  _  -> ()
  | Channel ic -> close_in ic

let get_parser parser : (module Parser) =
  match parser with
  | Legacy -> (module Legacy)
  | Sukerujo -> (module Sukerujo)

let handle input f =
  let (module P : Parser) = get_parser input.parser in
  let s = from input in
  try
    while true do P.handle (P.read s) f done
  with
  | Parsing.Parse_error
  | End_of_file                 -> ()

let parse input =
  let l = ref [] in
  handle input (fun e -> l := e::!l);
  List.rev !l
