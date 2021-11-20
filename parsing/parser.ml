open Kernel
open Basic

type stream = {md : Basic.mident; lexbuf : Lexing.lexbuf}

type from = Channel of in_channel | String of string
(* String of Dedukti code *)

type input = {file : string option; md : Basic.mident; from : from}

exception Parse_error of loc * string

let read str =
  try Menhir_parser.line Lexer.token str.lexbuf str.md
  with Menhir_parser.Error ->
    let loc = Lexer.get_loc str.lexbuf in
    let lex = Lexing.lexeme str.lexbuf in
    let msg = Format.sprintf "Unexpected token '%s'." lex in
    raise @@ Parse_error (loc, msg)

let lexing_from input =
  match input with
  | String s   -> Lexing.from_string s
  | Channel ic -> Lexing.from_channel ic

let md_of_file file =
  let open Filename in
  let base = basename file in
  let base = if check_suffix base ".dk" then chop_suffix base ".dk" else base in
  mk_mident base

let input_from_file file =
  let md = md_of_file file in
  let from = Channel (open_in file) in
  {file = Some file; from; md}

let input_from_stdin md = {file = None; from = Channel stdin; md}

let input_from_string md s = {file = None; from = String s; md}

let md_of_input t = t.md

let file_of_input t = t.file

let close input =
  match input.from with String _ -> () | Channel ic -> close_in ic

let from input =
  let md = input.md in
  {md; lexbuf = lexing_from input.from}

let handle input f =
  let s = from input in
  try
    while true do
      f (read s)
    done
  with Parsing.Parse_error | End_of_file -> ()

let parse input =
  let l = ref [] in
  handle input (fun e -> l := e :: !l);
  List.rev !l
