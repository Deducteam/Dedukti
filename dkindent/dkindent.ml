open Basic
open Parser
open Pp
open Reduction

let default_mident = ref None

let set_default_mident md = default_mident := Some md

module T = struct
  let mk_declaration _ i st ty =
    let st_str = match st with
      | Signature.Static -> ""
      | Signature.Definable -> "def "
    in
    Format.printf "@[<2>%s%a :@ %a.@]@.@." st_str print_ident i print_term ty

  let mk_definition _ i ty t = match ty with
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
    Format.printf "@[<v0>%a@].@.@." (print_list "" print_untyped_rule) l

  let mk_entry = function
  | Decl(lc,id,st,te) -> mk_declaration lc id st te
  | Def(lc,id,false,pty,te) -> mk_definition lc id pty te
  | Def(lc,id,true,pty,te) -> mk_opaque lc id pty te
  | Rules(rs) -> mk_rules rs
  | Eval(lc,red_cfg,  te) -> Format.printf "#WHNF%a@ %a." print_red_cfg red_cfg print_term te
  | Check(lc,false,false,Convert(te1,te2)) ->
    Format.printf "#CONV@ %a,@ %a." print_term te1 print_term te2
  | Check(lc,false,false,HasType(te,ty)) ->
    Format.printf "#CHECK@ %a,@ %a." print_term te print_term ty
  | Check _ -> failwith "unsupported command yet"
  | Infer(lc,red_cfg,te)  -> Format.printf "#INFER%a@ %a." print_red_cfg red_cfg print_term te
  | DTree (lc,m0,v)         ->
    begin match m0 with
      | None -> Format.printf "#GDT@ %a." print_ident v
      | Some m -> Format.printf "#GDT@ %a.%a." print_mident m print_ident v
    end
    | Print(lc, str) -> Format.printf "#PRINT %S." str
end

let process_chan md ic =
  Parser.handle_channel md T.mk_entry ic

let process_file file =
  (* Print.debug "Processing file %s\n" name; *)
  let ic = open_in file in
  let md =  Basic.mk_mident (match !default_mident with None -> file | Some str -> str) in
  process_chan md ic;
  close_in ic

let from_stdin = ref false
let files = ref []
let add_file f = files := f :: !files

let options =
  [ "-stdin", Arg.Set from_stdin, " read from stdin";
    "-default-rule", Arg.Set print_default, "print default name for rules without name";
    ("-module" , Arg.String set_default_mident     , "Give a default name to the current module")
  ]

let  _ =
  Arg.parse options add_file "usage: dkindent file [file...]";
  if !from_stdin then (
    let md = Basic.mk_mident (
        match !default_mident with
        | None -> Basic.debug 0 "[Warning] no module name given"; "stdin"
        | Some str -> str)
    in
    process_chan md stdin)
  else List.iter process_file (List.rev !files)
