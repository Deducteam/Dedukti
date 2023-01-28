(** Print the dependencies of a list of modules *)
open Kernel

open Api
open Basic
open Cmdliner

(** Output main program. This function assumes that all modules are in
   the [load_path]. *)
let output_deps :
    load_path:Api.Files.t ->
    Format.formatter ->
    Dep_legacy.t ->
    (unit, mident * exn) result =
 fun ~load_path fmt data ->
  let open Dep_legacy in
  let as_object_file_exn md =
    let file = Files.get_file_exn load_path Kernel.Basic.dloc md in
    let (File file) = Files.as_object_file file in
    file
  in
  let output_line : mident -> file_deps -> (unit, mident * exn) result =
   fun md deps ->
    try
      let file = Api.Files.get_file_exn load_path Kernel.Basic.dloc md in
      let (File obj_file) = Api.Files.as_object_file file in
      let (File file) = file in
      let deps = List.map as_object_file_exn (MidentSet.elements deps.deps) in
      let deps = String.concat " " deps in
      Format.fprintf fmt "%s : %s %s@." obj_file file deps;
      Ok ()
    with exn -> Error (md, exn)
  in
  Hashtbl.fold
    (fun md deps result ->
      match result with Ok () -> output_line md deps | Error err -> Error err)
    data (Ok ())

let output_sorted : Format.formatter -> Dep_legacy.t -> (unit, 'a) result =
 fun ppf data ->
  let deps = Dep_legacy.topological_sort data in
  Format.fprintf ppf "%s@." (String.concat " " deps);
  Ok ()

let dkdep config ignore output_file_opt sorted files =
  Config.init config;
  Dep_legacy.ignore := ignore;
  let load_path = Config.load_path config in
  if (not sorted) && ignore then
    Errors.fail_exit ~file:"<none>" ~code:"CLI" None
      "--ignore option can be used only with --sort option"
  else
    let output_fun = if sorted then output_sorted else output_deps ~load_path in
    let open Processor in
    let hook =
      {
        before = (fun _input _env -> ());
        after =
          (fun input _env exn ->
            match exn with
            | None -> ()
            | Some (_env, loc, exn) -> Errors.fail_exn input loc exn);
      }
    in
    (* Actual work. *)
    let deps = Processor.handle_files ~hook ~load_path ~files Dependencies in
    let oc = Option.fold ~none:stdout ~some:open_out output_file_opt in
    let fmt = Format.formatter_of_out_channel oc in
    match output_fun fmt deps with
    | Ok () ->
        Format.pp_print_flush fmt ();
        close_out oc
    | Error (md, exn) ->
        close_out oc;
        let code, _loc, exn_str =
          Errors.string_of_exception ~reduce:(fun x -> x) Kernel.Basic.dloc exn
        in
        let file =
          Format.asprintf "No file. While printing dependencies of module %a"
            pp_mident md
        in
        Errors.fail_exit ~file ~code:(string_of_int code) None
          "Internal error. Please repport the issue. Uncaught error:@.%s@."
          exn_str

let files =
  let doc = "Print dependencies of Dedukti file FILE" in
  Arg.(value & pos_all string [] & info [] ~docv:"FILE" ~doc)

let output =
  let doc = "Output to $(docv)" in
  Arg.(value & opt (some string) None & info ["output"; "o"] ~docv:"FILE" ~doc)

let sort =
  let doc = "Sort the source files according to their dependencies" in
  Arg.(value & flag & info ["sort"; "s"] ~doc)

let ignore =
  let doc = "Ignore not found dependencies" in
  Arg.(value & flag & info ["ignore"; "i"] ~doc)

let cmd_t = Term.(const dkdep $ Config.t $ ignore $ output $ sort $ files)

let cmd =
  let doc = "Dependency list generator for Dedukti files" in
  Cmd.(v (info "dep" ~version:"%%VERSION%%" ~doc) cmd_t)
