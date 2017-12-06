open Basic
open Ast
open Print

(* TO DO:cut the files in modules *)

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

let _ = Compiler.init_ast (mk_ident "arith")

let mk_prelude lc name =
  Env.init name

let mk_declaration lc id st pty : unit =
  match Env.declare lc id st pty with
  | OK () ->
    let name = mk_name (Env.get_name ()) id in
    let decl = Compiler.compile_declaration name pty in
    Compiler.add_declaration decl
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
    let name = mk_name (Env.get_name ()) id in
    let defn = Compiler.compile_definition name ty pte in
    Compiler.add_definition defn
  | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  match Env.define_op lc id pte pty_opt with
    | OK () -> failwith "not supported yet"
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Rule.Pattern (l,cst,_) -> (l,cst)
  | _ -> (dloc, mk_name (mk_mident "") qmark)

let mk_rules = Rule.( function
  | [] -> ()
  | (rule::_) as lst ->
      match Env.add_rules lst with
      | OK lst2 -> failwith "Rules are not handled"
      | Err e -> Errors.fail_env_error e
  )
let mk_command _ = failwith "Commands are not handled right now"

let export = ref false

let mk_ending () =
  ( if !export then
    if not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a'." pp_mident (Env.get_name ()) );
  Confluence.finalize ()
