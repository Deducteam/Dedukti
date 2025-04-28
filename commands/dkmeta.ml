open Kernel
open Basic
open Api

(* The main processor which normalises entries. *)
type _ Processor.t += Meta : unit Processor.t

let equal (type a b) :
    a Processor.t * b Processor.t ->
    (a Processor.t, b Processor.t) Processor.Registration.equal option =
  function
  | Meta, Meta -> Some (Processor.Registration.Refl Meta)
  | _ -> None

let meta config meta_debug meta_rules_files no_meta quoting no_unquoting
    register_before no_beta files =
  Config.init config;
  let encoding =
    Option.map
      Meta.(
        function
        | "lf" -> (module LF : ENCODING)
        | "prod" -> (module PROD : ENCODING)
        | "ltyped" -> (module APP : ENCODING)
        | s ->
            Errors.fail_exit ~file:"" ~code:"-1" (Some dloc)
              "Unknown encoding '%s'" s)
      quoting
  in
  if meta_debug then Debug.enable_flag Meta.debug_flag;
  if no_meta && meta_rules_files <> [] then
    Errors.fail_sys_error ~msg:"Incompatible options: '--no-meta' with '-m'" ();
  if no_meta && encoding <> None then
    Errors.fail_sys_error
      ~msg:"Incompatible options: '--no-meta' with '--encoding'" ();
  let load_path = Config.load_path config in
  let cfg =
    Meta.default_config ~beta:(not no_beta) ?encoding
      ~decoding:(not no_unquoting) ~register_before ~load_path ()
  in
  (* Adding normalisation will be done with an empty list of meta rules. *)
  if no_meta then Meta.add_rules cfg [];
  (match meta_rules_files with
  | [] -> ()
  | files ->
      let rules = Meta.parse_meta_files files in
      Meta.add_rules cfg rules;
      Meta.log "[SUCCESS] Meta rules registered successfuly");
  let post_processing env entry =
    let (module Printer) = Env.get_printer env in
    Format.printf "%a" Printer.print_entry entry
  in
  let hook =
    {
      Processor.before = (fun _ -> ());
      after =
        (fun env exn ->
          match exn with
          | None ->
              if not (Config.quiet config) then
                Meta.log "[SUCCESS] File '%s' was successfully metaified."
                  (Env.get_filename env)
          | Some (env, lc, exn) -> Env.fail_env_error env lc exn);
    }
  in
  let processor = Meta.make_meta_processor cfg ~post_processing in
  Processor.Registration.register_processor Meta {equal} processor;
  match config.Config.run_on_stdin with
  | None -> Processor.handle_files ~load_path ~files ~hook Meta
  | Some m ->
      let input = Parsers.Parser.input_from_stdin (Basic.mk_mident m) in
      Api.Processor.handle_input ~hook ~load_path ~input Meta

let meta_debug =
  let doc = "Activate meta-specific debug flag" in
  Cmdliner.Arg.(value & flag & info ["meta-debug"] ~doc)

let meta_rules_files =
  let doc = "Load meta rules from $(docv)" in
  Cmdliner.Arg.(value & opt_all file [] & info ["m"; "meta"] ~docv:"FILE" ~doc)

let no_meta =
  let doc = "Do not reduce terms" in
  Cmdliner.Arg.(value & flag & info ["no-meta"] ~doc)

let quoting =
  let doc = "[EXPERIMENTAL] Encoding of Dedukti files" in
  Cmdliner.Arg.(value & opt (some string) None & info ["quoting"] ~doc)

let no_unquoting =
  let doc =
    "[EXPERIMENTAL] Do not decode terms. Mainly for debugging purposes."
  in
  Cmdliner.Arg.(value & flag & info ["no-unquoting"] ~doc)

let register_before =
  let doc =
    "[EXPERIMENTAL] With a typed encoding, register entries before metafying \
     them."
  in
  Cmdliner.Arg.(value & flag & info ["register-before"] ~doc)

let no_beta =
  let doc = "Switch off beta while normalizing terms." in
  Cmdliner.Arg.(value & flag & info ["no-beta"] ~doc)

let files =
  let doc = "Dedukti files to process." in
  Cmdliner.Arg.(value & pos_all file [] & info [] ~docv:"FILE" ~doc)

let cmd_t =
  Cmdliner.Term.(
    const meta $ Config.t $ meta_debug $ meta_rules_files $ no_meta $ quoting
    $ no_unquoting $ register_before $ no_beta $ files)

let cmd =
  let doc = "Transform dk signatures using dk." in
  Cmdliner.Cmd.(v (info "meta" ~doc) cmd_t)
