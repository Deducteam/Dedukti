open Kernel
open Basic
open Api

let set_debug_mode opts =
  try Env.set_debug_mode opts
  with Env.DebugFlagNotRecognized c ->
    if c = 'a' then Debug.enable_flag Meta.debug_flag
    else raise (Env.DebugFlagNotRecognized c)

type _ Processor.t += Meta : unit Processor.t

let equal (type a b) :
    a Processor.t * b Processor.t ->
    (a Processor.t, b Processor.t) Processor.Registration.equal option =
  function
  | Meta, Meta -> Some (Processor.Registration.Refl Meta)
  | _          -> None

let version = "0.1"

let _ =
  let run_on_stdin = ref None in
  let beta = ref true in
  let switch_beta_off () = beta := false in
  let encoding : (module Meta.ENCODING) option ref = ref None in
  let set_encoding enc =
    if enc = "lf" then encoding := Some (module Meta.LF)
    else if enc = "prod" then encoding := Some (module Meta.PROD)
    else if enc = "ltyped" then encoding := Some (module Meta.APP)
    else
      Errors.fail_exit ~file:"" ~code:"-1" (Some dloc) "Unknown encoding '%s'"
        enc
  in
  let register_before = ref false in
  let encode_meta_rules = ref false in
  let decoding = ref true in
  let no_meta = ref false in
  let quiet = ref false in
  let meta_files = ref [] in
  let options =
    Arg.align
      [
        ( "-d",
          Arg.String set_debug_mode,
          " flags enables debugging for all given flags" );
        ( "-q",
          Arg.Unit
            (fun () ->
              quiet := true;
              Env.set_debug_mode "q"),
          " Quiet mode (equivalent to -d 'q'" );
        ("--no-color", Arg.Clear Errors.color, "");
        ( "-stdin",
          Arg.String (fun n -> run_on_stdin := Some n),
          " MOD Parses standard input using module name MOD" );
        ( "-version",
          Arg.Unit (fun () -> Format.printf "Meta Dedukti %s@." version),
          " Print the version number" );
        ( "-I",
          Arg.String Files.add_path,
          " DIR Add the directory DIR to the load path" );
        ( "-v",
          Arg.Unit (fun () -> set_debug_mode "a"),
          " Active the debug flag specific to dkmeta" );
        ( "-m",
          Arg.String (fun s -> meta_files := s :: !meta_files),
          " The file containing the meta rules." );
        ( "--no-meta",
          Arg.Unit (fun () -> no_meta := true),
          " Do not reduce terms." );
        ("--quoting", Arg.String set_encoding, " Encoding the Dedukti file.");
        ( "--no-quoting",
          Arg.Unit (fun () -> decoding := false),
          " Terms are not decoded after. Usage is mainly for debugging purpose."
        );
        ( "--quoting-meta",
          Arg.Unit (fun () -> encode_meta_rules := true),
          " Meta rules are also encoded. However this does not work with \
           product encoding" );
        ( "--register-before",
          Arg.Unit (fun () -> register_before := true),
          " With a typed encoding, entries are registered before they are \
           metaified" );
        ( "--no-beta",
          Arg.Unit switch_beta_off,
          " switch off beta while normalizing terms" );
      ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  if !no_meta && !meta_files <> [] then
    Errors.fail_sys_error ~msg:"Incompatible options: '--no-meta' with '-m'" ();
  let env =
    match !encoding with
    | None            -> None
    | Some (module E) ->
        let env =
          Env.init
            (Parsers.Parser.input_from_string (Basic.mk_mident "meta") "")
        in
        (* The signature of the encoding should be added to the current signature *)
        let sg = Env.get_signature env in
        Signature.import_signature sg E.signature;
        Some env
  in
  let cfg =
    Meta.
      {
        default_config with
        beta = !beta;
        encoding = !encoding;
        register_before = !register_before;
        encode_meta_rules = !encode_meta_rules;
        decoding = !decoding;
        env;
      }
  in
  let cfg =
    if !no_meta then {cfg with meta_rules = Some []}
    else
      let cfg = Meta.meta_of_files ~cfg !meta_files in
      if not !quiet then Errors.success "Meta files parsed.";
      cfg
  in
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
          | None                ->
              if not !quiet then
                Errors.success
                  (Format.asprintf "File '%s' was successfully metaified."
                     (Env.get_filename env))
          | Some (env, lc, exn) -> Env.fail_env_error env lc exn);
    }
  in
  let processor = Meta.make_meta_processor cfg ~post_processing in
  Processor.Registration.register_processor Meta {equal} processor;
  match !run_on_stdin with
  | None   -> Processor.handle_files files ~hook Meta
  | Some m ->
      let input = Parsers.Parser.input_from_stdin (Basic.mk_mident m) in
      Api.Processor.handle_input input ~hook Meta
