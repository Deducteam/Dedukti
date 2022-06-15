(** Print the dependencies of a list of modules *)
open Kernel

open Api
open Basic
open Cmdliner

(** Output main program. *)
let output_deps : Format.formatter -> Dep.t -> unit =
 fun oc data ->
  let open Dep in
  let objfile src = Filename.chop_extension src ^ ".dko" in
  let output_line : mident -> file_deps -> unit =
   fun _ deps ->
    let file = deps.file in
    let deps =
      List.map
        (fun md -> objfile (Files.get_file md))
        (MidentSet.elements deps.deps)
    in
    let deps = String.concat " " deps in
    try Format.fprintf oc "%s : %s %s@." (objfile file) file deps with _ -> ()
   (* Dependency is missing *)
  in
  Hashtbl.iter output_line data

let output_sorted : Format.formatter -> Dep.t -> unit =
 fun ppf data ->
  let deps = Dep.topological_sort data in
  Format.fprintf ppf "%s@." (String.concat " " deps)

let dkdep config ignore output sorted files =
  Config.init config;
  Dep.ignore := ignore;
  let output = match output with Some f -> open_out f | None -> stdout in
  let open Processor in
  let hook =
    {
      before = (fun _ -> ());
      after =
        (fun _ exn ->
          match exn with
          | None -> ()
          | Some (env, lc, exn) -> Env.fail_env_error env lc exn);
    }
  in
  (* Actual work. *)
  let deps = Processor.handle_files ~hook files Dependencies in
  let formatter = Format.formatter_of_out_channel output in
  let output_fun = if sorted then output_sorted else output_deps in
  output_fun formatter deps;
  Format.pp_print_flush formatter ();
  close_out output

let files =
  let doc = "Print dependencies of Dedukti file FILE" in
  Arg.(value & pos_all string [] & info [] ~docv:"FILE" ~doc)

let output =
  let doc = "Output to $(docv)" in
  Arg.(value & opt (some string) None & info ["output"; "o"] ~docv:"FILE" ~doc)

let sorted =
  let doc = "Sort the source files according to their dependencies" in
  Arg.(value & flag & info ["sort"; "s"] ~doc)

let ignore =
  let doc = "Ignore not found dependencies" in
  Arg.(value & flag & info ["ignore"; "i"] ~doc)

let cmd_t = Term.(const dkdep $ Config.t $ ignore $ output $ sorted $ files)

let cmd =
  let doc = "Dependency list generator for Dedukti files" in
  Cmd.(v (info "dep" ~version:"%%VERSION%%" ~doc) cmd_t)
