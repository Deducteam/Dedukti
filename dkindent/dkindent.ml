open Basics
open Pp

module T = struct
  let mk_prelude _ i = Format.printf "#NAME %a.@.@." print_ident i

  let mk_declaration _ i t =
    Format.printf "@[<2>%a :@ %a.@]@.@." print_ident i print_term t

  let mk_definition _ i ty t = match ty with
    | None -> Format.printf "@[<hv2>%a@ :=@ %a.@]@.@." print_ident i print_term t
    | Some ty ->
        Format.printf "@[<hv2>%a :@ %a@ :=@ %a.@]@.@."
          print_ident i print_term ty print_term t

  let mk_opaque _ i ty t = match ty with
    | None -> Format.printf "@[<hv2>{%a}@ :=@ %a.@]@.@." print_ident i print_term t
    | Some ty ->
        Format.printf "@[<hv2>{%a}@ :@ %a :=@ %a.@]@.@."
          print_ident i print_term ty print_term t

  let mk_rules l =
    Format.printf "@[<v0>%a@].@.@." (print_list "" print_rule) l

  let mk_command _ cmd =
    Format.printf "@[<2>%a@]@.@." Cmd.print_command cmd

  let mk_ending _ = ()
end

module P = Parser.Make(T)

let parse lb =
  try
    P.prelude Lexer.token lb ;
    while true do P.line Lexer.token lb done
  with
    | Tokens.EndOfFile -> ()
    | P.Error       -> Errors.fail (Lexer.get_loc lb)
                         "Unexpected token '%s'." (Lexing.lexeme lb)

let process_chan ic = parse (Lexing.from_channel ic)
let process_file name =
  (* Print.debug "Processing file %s\n" name; *)
  let ic = open_in name in
  process_chan ic;
  close_in ic

let from_stdin = ref false
let files = ref []
let add_file f = files := f :: !files

let options =
  [ "-stdin", Arg.Set from_stdin, " read from stdin"
  ]

let  _ =
  Arg.parse options add_file "usage: dkindent file [file...]";
  if !from_stdin
    then process_chan stdin
    else List.iter process_file (List.rev !files)
