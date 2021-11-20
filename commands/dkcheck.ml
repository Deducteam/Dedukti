open Kernel
open Api
open Cmdliner

(** Common options *)
type config = {no_color : bool; debug : string}

let no_color =
  let doc = "Disable colors in output" in
  let docs = Manpage.s_common_options in
  Arg.(value & flag & info ["no-color"] ~doc ~docs)

let debug =
  let docs = Manpage.s_common_options in
  let doc =
    "Enable debugging for flags in $(docv), overriding -q/--quiet. Available \
     flags: $(i,q) (quiet): disables all warnings, $(i,n) (notice): notifies \
     about which symbol or rule is currently treated, $(i,o) (module): \
     notifies about loading of an external module (associated to the command \
     #REQUIRE), $(i,c) (confluence): notifies about information provided to \
     the confluence checker (when option --confluence used), $(i,u) (rule): \
     provides information about type checking of rules, $(i,t) (typing): \
     provides information about type checking of terms, $(i,s) (subject \
     reduction) provides information about subject reduction checking of \
     terms, $(i,r) (reduce): provides information about reduction performed in \
     terms, $(i,m) (matching): provides information about pattern matching."
  in
  Arg.(value & opt string "" & info ["d"; "debug"] ~docv:"FLAGS" ~doc ~docs)

let quiet =
  let doc = "Do not print anything (same as -d q)" in
  let docs = Manpage.s_common_options in
  Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)

let verbose =
  let doc = "Verbose mode (equivalent to -d montru). Overrides -d/--debug." in
  let docs = Manpage.s_common_options in
  Arg.(value & flag & info ["v"; "verbose"] ~doc ~docs)

let config no_color debug quiet verbose =
  let debug =
    let d = ref "" in
    if quiet then d := "q";
    if debug <> "" then d := debug;
    if verbose then d := "montru";
    !d
  in
  {no_color; debug}

let config_t = Term.(const config $ no_color $ debug $ quiet $ verbose)

let init (c : config) : unit =
  (* Verbose overrides debug that overrides quiet. *)
  Env.set_debug_mode c.debug;
  Errors.color := not c.no_color

(** dkcheck options *)

let incl =
  let doc = "Add directory $(docv) to the load path" in
  Arg.(value & opt_all dir [] & info ["I"; "include"] ~docv:"DIR" ~doc)

let export =
  let doc =
    "Generate object files (\".dko\"). Object files are written in the same \
     directory as the source file."
  in
  Arg.(value & flag & info ["e"] ~doc)

let beautify =
  (* TODO put in a separate command. *)
  let doc = "Pretty print a file on the standard input" in
  Arg.(value & flag & info ["beautify"] ~doc)

let run_on_stdin =
  let doc = "Parses standard input using module name $(docv)" in
  Arg.(value & opt (some string) None & info ["stdin"] ~docv:"MOD" ~doc)

let de_bruijn =
  let doc = "Print de Bruijn indices in error messages" in
  Arg.(value & flag & info ["db"] ~doc)

let confluence =
  let doc =
    "Use $(docv) as external confluence checker. $(docv) must be a readable \
     file."
  in
  Arg.(
    value & opt (some string) None & info ["cc"; "confluence"] ~doc ~docv:"CMD")

let files =
  let doc = "Dedukti files to type check" in
  Arg.(value & pos_all string [] & info [] ~docv:"FILE" ~doc)

let eta =
  let doc = "Enable conversion modulo eta" in
  Arg.(value & flag & info ["eta"] ~doc)

let ll =
  let doc = "Check left linearity of rewrite rules" in
  Arg.(value & flag & info ["ll"] ~doc)

let sr_check =
  let doc =
    "Set the level of subject reduction checking to $(docv). Default value is \
     1, values < 0 may not terminate on rules that do not preserve typing."
  in
  Arg.(value & opt int 1 & info ["sr-check"] ~doc ~docv:"LVL")

let errors_in_snf =
  let doc = "Normalize terms in error messages" in
  Arg.(value & flag & info ["errors-in-snf"] ~doc)

let coc =
  let doc =
    "$(b,EXPERIMENTAL) Allows declaring symbols whose type contains Type in \
     the left-hand side of a product (similar to the logic of the Calculus of \
     Constructions)"
  in
  Arg.(value & flag & info ["coc"] ~doc)

let type_lhs =
  let doc = "Forbid rules with untypable left-hand side" in
  Arg.(value & flag & info ["type-lhs"] ~doc)

let dkcheck config confluence de_bruijn incl export beautify quiet run_on_stdin
    files eta ll sr_check errors_in_snf coc type_lhs =
  init config;
  Pp.print_db_enabled := de_bruijn;
  Option.iter Confluence.set_cmd confluence;
  List.iter Files.add_path incl;
  Env.check_ll := ll;
  Reduction.eta := eta;
  Env.check_arity := not eta;
  Srcheck.srfuel := sr_check;
  Env.errors_in_snf := errors_in_snf;
  Typing.coc := coc;
  Typing.fail_on_unsatisfiable_constraints := type_lhs;
  if beautify && export then (
    Format.eprintf "Beautify and export cannot be set at the same time@.";
    exit 2);
  let open Processor in
  let processor = if beautify then PrettyPrinter else TypeChecker in
  let hook_after env exn =
    match exn with
    | None              ->
        if (not beautify) && not quiet then
          Errors.success (Env.get_filename env);
        if export then Env.export env;
        Confluence.finalize ()
    | Some (env, lc, e) -> Env.fail_env_error env lc e
  in
  let hook =
    {before = (fun _ -> Confluence.initialize ()); after = hook_after}
  in
  Processor.handle_files files ~hook processor;
  match run_on_stdin with
  | None   -> ()
  | Some m ->
      let input = Parsers.Parser.input_from_stdin (Basic.mk_mident m) in
      Processor.handle_input input processor

let cmd =
  let doc = "Type check a list of Dedukti files" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Minimal proof checker for the lambdaPi calculus modulo rewriting. For \
         more information, see https://github.com/Deducteam/Dedukti.";
      `S Manpage.s_examples;
      `P "Given a Dedukti file $(i,examples/append.dk), the command";
      `Pre "dkcheck examples/append.dk";
      `P "should exit with 0 and output (on stderr)";
      `Pre "[SUCCESS] examples/append.dk was successfully checked.";
      `S Manpage.s_bugs;
      `P "Report bugs to <dedukti-dev@inria.fr>.";
    ]
  in
  ( Term.(
      const dkcheck $ config_t $ confluence $ de_bruijn $ incl $ export
      $ beautify $ quiet $ run_on_stdin $ files $ eta $ ll $ sr_check
      $ errors_in_snf $ coc $ type_lhs),
    Term.info "dkcheck" ~version:"%%VERSION%%" ~doc ~exits ~man )

let () = Term.(exit @@ eval cmd)
