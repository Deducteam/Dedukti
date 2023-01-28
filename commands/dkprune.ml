open Kernel
open Parsers
open Api
open Basic
module NSet = Basic.NameSet
module MSet = Basic.MidentSet
module Printer = Pp.Default

let d_prune = Debug.register_flag "Dkprune"

let enable_log : unit -> unit = fun () -> Debug.enable_flag d_prune

type dkprune_error = BadFormat of Basic.loc | NoDirectory

let fail_dkprune_error err =
  match err with
  | BadFormat lc ->
      ( 1000,
        Some lc,
        Format.asprintf "Only commands #GDT and #REQUIRE are authorized" )
  | NoDirectory ->
      ( 1001,
        None,
        Format.asprintf
          "A directory output has to be specified (option -o, see --help)" )

exception Dkprune_error of dkprune_error

let fail_dkprune ~reduce:_ exn =
  match exn with
  | Dkprune_error err -> Some (fail_dkprune_error err)
  | _ -> None

let _ = Errors.register_exception fail_dkprune

let output_directory : string option ref = ref None

let gre fmt = "\027[32m" ^^ fmt ^^ "\027[0m%!"

let log fmt = Debug.debug d_prune (gre fmt)

let _ =
  Dep_legacy.ignore := true;
  (* If a dependency is missing this does not trigger an exception
     because we do not want to compute dependencies from a logic file
  *)
  Dep_legacy.compute_all_deps := true

(* We want to compute items dependencies *)

(* Get the output file from the input file *)
let output_file : string -> string =
 fun file ->
  let basename = Filename.basename file in
  match !output_directory with
  | None -> raise @@ Dkprune_error NoDirectory
  | Some dir -> Filename.concat dir basename

(* Memoize the modules already computed *)
let computed = ref MSet.empty

let name_of_entry md = function
  | Entry.Decl (_, id, _, _, _) -> Some (Basic.mk_name md id)
  | Entry.Def (_, id, _, _, _, _) -> Some (Basic.mk_name md id)
  | Entry.Rules (_, r :: _) ->
      let open Rule in
      let r' = to_rule_infos r in
      Some r'.cst
  | _ -> None

(* Wrapper around Processor.Dependencies to avoid to compute dependencies of a module already computed *)
module PruneDepProcessor : Processor.S with type output = unit = struct
  type t = Env.t

  type output = unit

  let handle_entry env entry =
    let md = Env.get_name env in
    let (module Dep) = Processor.get_deps in
    if not @@ MSet.mem md !computed then ignore (Dep.handle_entry env entry)

  let handle_entry env entry = handle_entry env entry; env

  let output _ = ()
end

(* Gather all the identifiers declared or defined in a module *)
module GatherNames : Processor.S with type output = NSet.t = struct
  type t = Env.t

  type output = NSet.t

  let names = ref NSet.empty

  let handle_entry env =
    let md = Env.get_name env in
    let add_name n = names := NSet.add n !names in
    fun entry ->
      match name_of_entry md entry with None -> () | Some md -> add_name md

  let handle_entry env entry = handle_entry env entry; env

  let output _ = !names
end

(* Add all the names that should be kept as outpud. Either an entire module or a specific name *)
module ProcessConfigurationFile : Processor.S with type output = NSet.t = struct
  type t = Env.t

  type output = NSet.t

  let names = ref NSet.empty

  let handle_entry env = function
    | Entry.Require (loc, md) ->
        let load_path = Env.get_load_path env in
        let (File file) = Files.get_file_exn load_path loc md in
        (* Load path is not needed since no importation is done by the
           [GatherNames] processor. *)
        let load_path = Files.empty in
        let snames =
          Processor.handle_files load_path ~files:[file] (module GatherNames)
        in
        names := NSet.union snames !names
    | Entry.DTree (_, Some md, id) -> names := NSet.add (mk_name md id) !names
    | _ as e -> raise @@ Dkprune_error (BadFormat (Entry.loc_of_entry e))

  let handle_entry env entry = handle_entry env entry; env

  let output _ = !names
end

(* This is called on each module which appear in the configuration
   files. This is called also on each module which is in the
   transitive closure of the module dependencies *)
let rec run_on_files ~load_path files =
  let before input env =
    let md = Env.get_name env in
    if not @@ MSet.mem md !computed then
      match Parser.file_of_input input with
      | None ->
          log "[COMPUTE DEP] %s" (string_of_mident (Parser.md_of_input input))
      | Some file -> log "[COMPUTE DEP] %s" file
  in
  let after input env exn =
    match exn with
    | None ->
        let md = Env.get_name env in
        computed := MSet.add md !computed;
        let deps = Hashtbl.find Dep_legacy.deps md in
        let add_files md files =
          if MSet.mem md !computed then files
          else
            let (File file) = Files.get_file_exn load_path Basic.dloc md in
            file :: files
        in
        let new_files = MSet.fold add_files deps.deps [] in
        if List.length new_files <> 0 then run_on_files ~load_path new_files
    | Some (_env, loc, exn) -> Errors.fail_exn input loc exn
  in
  let hook = Processor.{before; after} in
  (* Load path is not needed since no importation is done by the
     [PruneDepProcessor] processor. *)
  let load_path = Files.empty in
  Processor.handle_files ~hook load_path ~files (module PruneDepProcessor)

(* compute dependencies for each module which appear in the configuration files *)
let handle_modules load_path mds =
  let files =
    List.map
      (fun md ->
        let (File file) = Files.get_file_exn load_path Basic.dloc md in
        file)
      mds
  in
  run_on_files ~load_path files

(* compute dependencies for eaach name which appear in the configuration files *)
let handle_names load_path names =
  let open Basic.MidentSet in
  let mds = NameSet.fold (fun name s -> add (Basic.md name) s) names empty in
  handle_modules load_path (elements mds)

(* check if all the entry of a file are pruned *)
let is_empty deps file =
  (* Load path is not needed since no importation is done by the
     [GatherNames] processor. *)
  let names =
    Processor.handle_files Files.empty ~files:[file] (module GatherNames)
  in
  NSet.is_empty (NSet.inter names deps)

(* for each input file for which dependencies has been computed, we write an output file *)
let write_file deps in_file =
  let out_file = output_file in_file in
  log "[WRITING FILE] %s" out_file;
  if not (is_empty deps in_file) then (
    let input = Parser.from_file ~file:in_file in
    let md = Parser.md_of_input input in
    let output = open_out out_file in
    let fmt = Format.formatter_of_out_channel output in
    let handle_entry e =
      let name = name_of_entry md e in
      match name with
      | None -> Format.fprintf fmt "%a" Printer.print_entry e
      | Some name ->
          if NSet.mem name deps then
            Format.fprintf fmt "%a" Printer.print_entry e
    in
    Parser.to_seq_exn input |> Seq.iter handle_entry;
    Parser.close input;
    close_out output)

(* print_dependencies for all the names which are in the transitive closure of names specificed in the configuration files *)
let print_dependencies ~load_path names =
  let open Dep_legacy in
  (* We fake an environment to print an error *)
  try
    NameSet.iter Dep_legacy.transitive_closure names;
    let down_deps =
      NameSet.fold
        (fun name dependencies ->
          NameSet.union (get_data name).down dependencies)
        names names
    in
    let mds =
      Hashtbl.fold (fun md _ set -> MSet.add md set) Dep_legacy.deps MSet.empty
    in
    let in_files md files =
      match Files.file_status load_path md with
      | Files.File_not_found | Files.File_found_multiple_time _ -> files
      | Files.File_found_once (File file) ->
          if is_empty down_deps file then files else file :: files
    in
    let in_files = MSet.fold in_files mds [] in
    List.iter (write_file down_deps) in_files
  with
  | Dep_legacy.Dep_error (NameNotFound name) ->
      Errors.fail_exit ~file:"configuration file" ~code:"DKPRUNE" None
        "The name %a does not exists@." Pp.Default.print_name name
  | exn ->
      (* Ugly!!! *)
      let input =
        Parsers.Parser.from_string
          (Kernel.Basic.mk_mident "configuration file")
          ""
      in
      Errors.fail_exn input Basic.dloc exn

let files =
  let doc = "Dedukti files to process" in
  Cmdliner.Arg.(value & pos_all string [] & info [] ~docv:"FILE" ~doc)

let output =
  let doc = "Output files in directory $(docv)" in
  Cmdliner.Arg.(
    value & opt (some dir) None & info ["o"; "output"] ~doc ~docv:"DIR")

let log =
  let doc = "Print log" in
  Cmdliner.Arg.(value & flag & info ["l"; "log"] ~doc)

let prune config log output files =
  Config.init config;
  if log then enable_log ();
  output_directory := output;
  let run_on_constraints files =
    (* Load path is not needed since no importation is done by the
       [GatherNames] processor. *)
    let load_path = Files.empty in
    Processor.handle_files load_path ~files (module ProcessConfigurationFile)
  in
  let names = run_on_constraints files in
  let load_path = Config.load_path config in
  handle_names load_path names;
  print_dependencies ~load_path names

let cmd_t = Cmdliner.Term.(const prune $ Config.t $ log $ output $ files)

let cmd =
  let doc = "Compute dependencies of a set of Dedukti files." in
  Cmdliner.Cmd.(v (info "prune" ~version:"%%VERSION%%" ~doc) cmd_t)
