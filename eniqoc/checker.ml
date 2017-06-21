open Basic
open Ast
open Print
(* ********************************* *)

let verbose = ref false

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c;
    Format.kfprintf (fun _ -> prerr_newline () ) Format.err_formatter fmt
  ) else
    Format.ifprintf Format.err_formatter fmt

(* ********************************* *)

let _ =   Format.printf "%a@." Print.print_prelude ()

let mk_prelude lc name =
  Env.init name

let mk_declaration lc id st pty : unit =
  match Env.declare lc id st pty with
  | OK () ->
    let decl = Compiler.compile_declaration (Env.get_name ()) id pty in
    Format.printf "%a@." Print.print_obj decl
  | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  let ty =
    match pty_opt with
    | None -> failwith "definition untyped are not supported now"
    | Some ty -> ty
  in
  match Env.define lc id pte pty_opt with
  | OK () ->
    let decl = Compiler.compile_definition (Env.get_name ()) id ty pte in
    Format.printf "%a@." Print.print_obj decl
  | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  match Env.define_op lc id pte pty_opt with
    | OK () -> failwith "not supported yet"
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Rule.Pattern (l,md,id,_) -> (l,md,id)
  | _ -> (dloc,qmark,qmark)

let mk_rules = Rule.( function
  | [] -> ()
  | (rule::_) as lst ->
      match Env.add_rules lst with
      | OK lst2 -> failwith "Rules are not handled"
      | Err e -> Errors.fail_env_error e
  )
let mk_command = Cmd.mk_command

let export = ref false

let mk_ending () =
  ( if !export then
    if not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a'." pp_ident (Env.get_name ()) );
  Confluence.finalize ()
