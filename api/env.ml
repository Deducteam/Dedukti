open Kernel
open Basic
open Term
open Rule
open Typing

exception DebugFlagNotRecognized of char

let set_debug_mode =
  String.iter (function
    | 'q' -> Debug.disable_flag Debug.d_warn
    | 'n' -> Debug.enable_flag Debug.d_notice
    | 'o' -> Debug.enable_flag Signature.d_module
    | 'c' -> Debug.enable_flag Confluence.d_confluence
    | 'u' -> Debug.enable_flag Typing.d_rule
    | 't' -> Debug.enable_flag Typing.d_typeChecking
    | 's' -> Debug.enable_flag Srcheck.d_SR
    | 'r' -> Debug.enable_flag Reduction.d_reduce
    | 'm' -> Debug.enable_flag Matching.d_matching
    | c -> raise (DebugFlagNotRecognized c))

type t = {
  load_path : Files.load_path;
      (* Directories where object files can be found. *)
  sg : Signature.t;
  red : (module Reduction.S);
  typer : (module Typing.S);
}

let dummy ?(md = Basic.mk_mident "") () =
  let dummy_sig = Signature.make ~explicit_import:false md (fun _ _ -> "") in
  {
    load_path = Files.empty;
    sg = dummy_sig;
    red = (module Reduction.Default);
    typer = (module Typing.Default);
  }

let get_load_path env = env.load_path

let check_arity = ref true

let check_ll = ref false

let explicit_import = ref false

let init load_path md =
  let find_object_file = Files.find_object_file_exn load_path in
  let explicit_import = !explicit_import in
  let sg = Signature.make ~explicit_import md find_object_file in
  let red : (module Reduction.S) = (module Reduction.Default) in
  let typer : (module Typing.S) = (module Typing.Default) in
  {load_path; sg; red; typer}

let set_reduction_engine env (module R : Reduction.S) =
  let red = (module R : Reduction.S) in
  let typer = (module Typing.Make (R) : Typing.S) in
  {env with red; typer}

let get_reduction_engine env = env.red

let get_name env = Signature.get_name env.sg

let get_signature env = env.sg

let get_printer env : (module Pp.Printer) =
  (module Pp.Make (struct
    let get_name () = get_name env
  end))

module HName = Hashtbl.Make (struct
  type t = name

  let equal = name_eq

  let hash = Hashtbl.hash
end)

let get_symbols env =
  let table = HName.create 11 in
  Signature.iter_symbols (fun md id -> HName.add table (mk_name md id)) env.sg;
  table

let get_type env lc cst = Signature.get_type env.sg lc cst

let get_dtree env lc cst = Signature.get_dtree env.sg lc cst

let export env ~file =
  let (File file) = Files.as_object_file file in
  let oc = open_out file in
  Signature.export env.sg oc;
  close_out oc;
  (* FIXME: Not closed if Signature raises an error *)
  close_out oc

let import env lc md = Signature.import env.sg lc md

let is_injective env lc cst = Signature.is_injective env.sg lc cst

let is_static env lc cst = Signature.is_static env.sg lc cst

let _add_rules env rs =
  let ris = List.map Rule.to_rule_infos rs in
  if !check_arity then List.iter Rule.check_arity ris;
  if !check_ll then List.iter Rule.check_linearity ris;
  Signature.add_rules env.sg ris

let _declare env lc (id : ident) scope st ty : unit =
  let (module T) = env.typer in
  Signature.add_declaration env.sg lc id scope st
    (match (T.inference env.sg ty, st) with
    | Kind, Definable AC | Kind, Definable (ACU _) ->
        raise (Typing_error (SortExpected (ty, [], mk_Kind)))
    | Type _, Definable AC -> mk_Arrow dloc ty (mk_Arrow dloc ty ty)
    | Type _, Definable (ACU neu) ->
        ignore (T.checking env.sg neu ty);
        mk_Arrow dloc ty (mk_Arrow dloc ty ty)
    | Kind, _ | Type _, _ -> ty
    | s, _ -> raise (Typing_error (SortExpected (ty, [], s))))

let declare env lc id scope st ty : unit = _declare env lc id scope st ty

let _define env lc (id : ident) (scope : Signature.scope) (opaque : bool)
    (te : term) (ty_opt : Typing.typ option) : unit =
  let (module T) = env.typer in
  let ty =
    match ty_opt with
    | None -> T.inference env.sg te
    | Some ty -> T.checking env.sg te ty; ty
  in
  match ty with
  | Kind -> raise @@ Typing_error (InexpectedKind (te, []))
  | _ ->
      if opaque then
        Signature.add_declaration env.sg lc id scope Signature.Static ty
      else
        let _ =
          Signature.add_declaration env.sg lc id scope
            (Signature.Definable Free) ty
        in
        let cst = mk_name (get_name env) id in
        let rule =
          {name = Delta cst; ctx = []; pat = Pattern (lc, cst, []); rhs = te}
        in
        _add_rules env [rule]

let define env lc id scope op te ty_opt : unit =
  _define env lc id scope op te ty_opt

let add_rules env (rules : partially_typed_rule list) :
    (Exsubst.ExSubst.t * typed_rule) list =
  let (module T) = env.typer in
  let rs2 = List.map (T.check_rule env.sg) rules in
  _add_rules env rules; rs2

let infer env ?(ctx = []) te =
  let (module T) = env.typer in
  let ty = T.infer env.sg ctx te in
  (* We only verify that [ty] itself has a type (that we immediately
     throw away) if [ty] is not [Kind], because [Kind] does not have a
     type, but we still want [infer ctx Type] to produce [Kind] *)
  if ty <> mk_Kind then ignore (T.infer env.sg ctx ty);
  ty

let check env ?(ctx = []) te ty =
  let (module T) = env.typer in
  T.check env.sg ctx te ty

let _unsafe_reduction env red te =
  let (module R) = env.red in
  R.reduction red env.sg te

let _reduction env ctx red te =
  (* This is a safe reduction, so we check that [te] has a type
     before attempting to normalize it, but we only do so if [te]
     is not [Kind], because [Kind] does not have a type, but we
     still want to be able to reduce it *)
  let (module T) = env.typer in
  if te <> mk_Kind then ignore (T.infer env.sg ctx te);
  _unsafe_reduction env red te

let reduction env ?(ctx = []) ?(red = Reduction.default_cfg) te =
  _reduction env ctx red te

let unsafe_reduction env ?(red = Reduction.default_cfg) te =
  _unsafe_reduction env red te

let are_convertible env ?(ctx = []) te1 te2 =
  let (module T) = env.typer in
  let (module R) = env.red in
  let ty1 = T.infer env.sg ctx te1 in
  let ty2 = T.infer env.sg ctx te2 in
  R.are_convertible env.sg ty1 ty2 && R.are_convertible env.sg te1 te2
