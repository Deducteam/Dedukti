open Kernel
open Basic

type from = Channel of in_channel | String of string
(* String of Dedukti code *)

type kind = [`File of string | `Stdin | `String]

type 'kind input = {
  kind : 'kind;
  md : Basic.mident;
  from : from;
}
  constraint 'kind = [< kind]

type error =
  | E : {loc : loc; input : 'kind input; lexbuf : Lexing.lexbuf} -> error

let loc_of_error (E {loc; _}) = loc

let lexbuf_of_error (E {lexbuf; _}) = lexbuf

let read input lexbuf md =
  match Menhir_parser.line Lexer.token lexbuf md with
  | exception Menhir_parser.Error ->
      let loc = Lexer.get_loc lexbuf in
      Error (E {loc; input; lexbuf})
  | exception End_of_file -> Ok None
  | entry -> Ok (Some entry)

let lexing_from input =
  match input with
  | Channel ic -> Lexing.from_channel ic
  | String s -> Lexing.from_string s

let md_of_file file =
  let open Filename in
  let base = basename file in
  let base = if check_suffix base ".dk" then chop_suffix base ".dk" else base in
  mk_mident base

let from_file ~file : [> `File of string] input =
  let md = md_of_file file in
  let from = Channel (open_in file) in
  {kind = `File file; from; md}

let from_stdin md : [> `Stdin] input = {kind = `Stdin; from = Channel stdin; md}

let from_string md s : [> `String] input = {kind = `String; from = String s; md}

let md_of_input t = t.md

let file_of_input (t : [`File of string] input) : string =
  match t.kind with `File file -> file

let kind_of_input (input : 'kind input) : 'kind = input.kind

let close input =
  match input.from with String _ -> () | Channel ic -> close_in ic

let unfold input md lexbuf =
  match read input lexbuf md with
  | Ok None -> None
  | Ok (Some entry) -> Some (Ok entry, lexbuf)
  | Error err -> Some (Error err, lexbuf)

type ('kind, 'entries) unit = {
  md : Kernel.Basic.mident;
  kind : 'kind;
  entries : 'entries Seq.t;
}

let to_unit input =
  let lexbuf = lexing_from input.from in
  let kind = kind_of_input input in
  let md = input.md in
  let entries = Seq.unfold (unfold input md) lexbuf in
  {md; kind; entries}

exception Parser_error of error

let raise_on_error unit =
  {
    unit with
    entries =
      Seq.map
        (function
          | Ok entry -> entry
          | Error (E {input; _} as error) ->
              close input;
              raise @@ Parser_error error)
        unit.entries;
  }

let units_of_files ~files =
  let seq = List.to_seq files in
  let input_seq = Seq.map (fun file -> from_file ~file) seq in
  Seq.map (fun input -> to_unit input) input_seq
