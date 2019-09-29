open Basic

module NSet = Basic.NameSet
module MSet = Basic.MidentSet

module Printer = Pp.Default


exception NoDirectory
exception EntryNotHandled of Entry.entry
exception BadFormat
exception NoPruneFile

let output_directory : string option ref = ref None

module D = Basic.Debug
type D.flag += D_prune
let _ = D.register_flag D_prune "Dkprune"
let enable_log : unit -> unit = fun () -> D.enable_flag D_prune

let gre fmt = "\027[32m" ^^ fmt ^^ "\027[0m%!"

let log fmt = D.debug D_prune (gre fmt)

type constraints =
  {
    names:Basic.NameSet.t;
    mds: mident list
  }

let _ =
  Dep.ignore := true;
  Dep.compute_all_deps := true

let output_file : string -> string = fun file ->
  let basename = Filename.basename file in
  match !output_directory with
  | None -> raise @@ NoDirectory
  | Some dir -> Filename.concat dir basename

let computed = ref MSet.empty

let name_of_entry md = function
    | Entry.Decl(_,id,_,_) ->
      Some (Basic.mk_name md id)
    | Entry.Def(_,id,_,_,_) ->
      Some (Basic.mk_name md id)
    | Entry.Rules(_,r::_) ->
      let open Rule in
      let r' = to_rule_infos r in
      Some r'.cst
    | _ -> None

module PruneDepProcessor : Processor.S with type t = unit =
struct

  type t = unit

  let handle_entry env entry =
    let md = Env.get_name env in
    if not @@ MSet.mem md !computed then
      Processor.Dependencies.handle_entry env entry

  let get_data () = ()
end

module GatherNames : Processor.S with type t = NSet.t =
struct

  type t = NSet.t

  let names = ref NSet.empty

  let handle_entry env =
    let md = Env.get_name env in
    let add_name n = names := NSet.add n !names in
    fun entry ->
      match name_of_entry md entry with
      | None -> raise @@ EntryNotHandled entry
      | Some md -> add_name md

  let get_data () = !names
end

module ProcessConfigurationFile : Processor.S with type t = NSet.t =
struct

  type t = NSet.t

  let names = ref NSet.empty

  let handle_entry _ =
    function
    | Entry.Require(_,md) ->
      let file = Files.get_file md in
      let snames = Processor.handle_files [file] (module GatherNames) in
      names := NSet.union snames !names
    | Entry.DTree(_,Some md, id) -> names := NSet.add (mk_name md id) !names
    | _ -> raise BadFormat

  let get_data () = !names

end

let rec run_on_files files =
  let hook_before env =
    match Parser.file_of_input (Env.get_input env) with
    | None      -> log "[COMPUTE DEP] %s" (string_of_mident (Parser.md_of_input (Env.get_input env)))
    | Some file -> log "[COMPUTE DEP] %s" file
  in
  let hook_after env exn =
    match exn with
    | None ->
      let md = Env.get_name env in
      computed := MSet.add md !computed;
      let deps = Hashtbl.find Dep.deps md in
      let add_files md files = if MSet.mem md !computed then files else (Files.get_file md)::files in
      let new_files = MSet.fold add_files deps.deps [] in
      if List.length new_files <> 0 then run_on_files new_files
    | Some (env, lc, e) -> Errors.fail_env_error env lc e
  in
  Processor.handle_files files ~hook_before ~hook_after (module PruneDepProcessor)

let handle_constraints mds =
  let files = List.map Files.get_file mds in
  run_on_files files

let is_empty deps file =
  let names = Processor.handle_files [file] (module GatherNames) in
  NSet.is_empty (NSet.inter names deps)

let write_file deps in_file =
  let out_file = output_file in_file in
  log "[WRITING FILE] %s" out_file;
  if not (is_empty deps in_file)
  then
    let input  = Parser.input_from_file in_file in
    let md     = Parser.md_of_input input in
    let output = open_out out_file in
    let fmt    = Format.formatter_of_out_channel output in
    let handle_entry e =
      let name = name_of_entry md e in
      match name with
      | None   -> raise @@ EntryNotHandled e
      | Some name -> if NSet.mem name deps then Format.fprintf fmt "%a" Printer.print_entry e
    in
    Parser.handle input handle_entry;
    Parser.close input;
    close_out output

let print_dependencies names =
  let open Dep in
  NameSet.iter Dep.transitive_closure names;
  let down_deps = NameSet.fold
      (fun name dependencies -> NameSet.union (get_data name).down dependencies) names names in
  let mds = Hashtbl.fold (fun md _ set -> MSet.add md set) Dep.deps MSet.empty in
  let in_files md files =
    let file = Files.get_file md in
    if is_empty down_deps file then files else file::files
  in
  let in_files = MSet.fold in_files mds [] in
  List.iter (write_file down_deps) in_files

let _ =
  let args = Arg.align
    [ ( "-l"
      , Arg.Unit enable_log
      , " Print log")
    ; ( "-I"
      , Arg.String Files.add_path
      , " DIR Add the directory DIR to the load path" )
    ; ( "-o"
      , Arg.String (fun s -> output_directory := Some s)
      , " Set the output directory" ) ]
  in
  let usage = Format.sprintf "Usage: %s [OPTION]... [FILE]...
Compute the dependencies of the given Dedukti FILE(s).
For more information see https://github.com/Deducteam/Dedukti.
Available options:" Sys.argv.(0) in
  let files =
    let files = ref [] in
    Arg.parse args (fun f -> files := f :: !files) usage;
    !files
  in
  let run_on_constraints files = Processor.handle_files files (module ProcessConfigurationFile) in
  let names = run_on_constraints files in
  print_dependencies names
