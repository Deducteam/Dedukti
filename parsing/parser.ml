open Kernel
open Basic

type from = Channel of in_channel | String of string
(* String of Dedukti code *)

type input = {file : string option; md : Basic.mident; from : from}

type error = {loc : loc; lexbuf : Lexing.lexbuf}

let read lexbuf md =
  match Menhir_parser.line Lexer.token lexbuf md with
  | exception Menhir_parser.Error ->
      let loc = Lexer.get_loc lexbuf in
      Error {loc; lexbuf}
  | exception End_of_file -> Ok None
  | entry -> Ok (Some entry)

let lexing_from input =
  match input with
  | String s -> Lexing.from_string s
  | Channel ic -> Lexing.from_channel ic

let md_of_file file =
  let open Filename in
  let base = basename file in
  let base = if check_suffix base ".dk" then chop_suffix base ".dk" else base in
  mk_mident base

let from_file ~file =
  let md = md_of_file file in
  let from = Channel (open_in file) in
  {file = Some file; from; md}

let from_stdin md = {file = None; from = Channel stdin; md}

let from_string md s = {file = None; from = String s; md}

let md_of_input t = t.md

let file_of_input t = t.file

let close input =
  match input.from with String _ -> () | Channel ic -> close_in ic

let rec to_seq md lexbuf =
  match read lexbuf md with
  | Ok None -> Seq.empty
  | Ok (Some entry) -> Seq.cons (Ok entry) (to_seq md lexbuf)
  | Error err -> Seq.cons (Error err) (to_seq md lexbuf)

let to_seq input =
  let md = input.md in
  let lexbuf = lexing_from input.from in
  to_seq md lexbuf

exception Parser_error of error

let to_seq_exn input =
  to_seq input
  |> Seq.map (function
       | Ok entry -> entry
       | Error error -> raise @@ Parser_error error)
