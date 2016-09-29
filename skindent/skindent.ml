open Basics
open Pp

let pmodule = ref false
let print_ident = Pp.print_ident
let cpt_module = ref 0

let is_module id =
  let s = string_of_ident id in
  String.length s > 9 && String.sub s 0 9 = "NEWMODULE"
let is_end_module id =
  let s = string_of_ident id in
  String.length s > 9 && String.sub s 0 9 = "ENDMODULE"

let module_of_ident id =
  let s = string_of_ident id in
  if !Pp.resugar then
    let s' = String.sub s 10 (String.length s - 10) in
    Format.printf "@[#NEWMODULE %s.@]@." s'
  else
    ()
let end_module_of_ident id =
  if !Pp.resugar then
    Format.printf "@[#ENDMODULE.@]@."
  else
    ()

module T = struct
  let mk_prelude _ i = Format.printf "#NAME %a.@.@." print_ident i

  let mk_declaration _ i t =
    Format.printf "@[<2>%a :@ %a.@]@.@." print_ident i print_term t

  let mk_definable _ i t =
    if is_module i then
      module_of_ident i
    else if is_end_module i then
      end_module_of_ident i
    else
      Format.printf "@[<2>def %a :@ %a.@]@.@." print_ident i print_term t

  let mk_definition _ i ty t =
    match ty with
    | None -> Format.printf "@[<hv2>def %a@ :=@ %a.@]@.@." print_ident i print_term t
    | Some ty ->
      Format.printf "@[<hv2>def %a :@ %a@ :=@ %a.@]@.@."
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

  let mk_module id =
    if !Pp.resugar then
      Format.printf "@[#NEWMODULE %a.@]@." print_ident id
    else if !pmodule then
      Format.printf "@[def NEWMODULE_%a : Type.@]@." print_ident id
    else
      ()

  let mk_endmodule () =
    if !Pp.resugar then
          Format.printf "@[#ENDMODULE.@]@."
    else if !pmodule then
      begin
        incr cpt_module;
        Format.printf "@[def ENDMODULE_%d : Type.@]@." !cpt_module
      end
    else
      ()

  let mk_ending () = ()
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

let () = resugar := false

let options =
  [ "-stdin", Arg.Set from_stdin, " read from stdin";
    "-resugar", Arg.Set resugar, " use syntactic sugar in output";
    "-module", Arg.Set pmodule, " add module declaration as definition in dedukti"
  ]

let  _ =
  Arg.parse options add_file "usage: skindent file [file...]";
  if !from_stdin
    then process_chan stdin
    else List.iter process_file (List.rev !files)
