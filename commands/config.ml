(** Options shared by all commands *)
open Api

open Cmdliner

type t = {
  load_path : Api.Files.t;
  load_path_legacy : string list;
  run_on_stdin : string option;
  no_color : bool;
  debug : string;
}

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

let run_on_stdin =
  let doc = "Parses standard input using module name $(docv)" in
  let docs = Manpage.s_common_options in
  Arg.(value & opt (some string) None & info ["stdin"] ~docv:"MOD" ~doc ~docs)

let included_directories =
  let doc = "Add directory $(docv) to the load path" in
  let docs = Manpage.s_common_options in
  Arg.(value & opt_all dir [] & info ["I"; "include"] ~docv:"DIR" ~doc ~docs)

let config no_color debug included_directories run_on_stdin quiet verbose =
  let debug =
    let d = ref "" in
    if quiet then d := "q";
    if debug <> "" then d := debug;
    if verbose then d := "montru";
    !d
  in
  let load_path_legacy = included_directories in
  let load_path =
    Api.Files.(List.fold_left add_path empty included_directories)
  in
  {load_path; run_on_stdin; no_color; debug; load_path_legacy}

let t : t Term.t =
  Term.(
    const config $ no_color $ debug $ included_directories $ run_on_stdin
    $ quiet $ verbose)

(** [init c] sets up environment with configuration [c]. *)
let init (c : t) : unit =
  (* Verbose overrides debug that overrides quiet. *)
  Env.set_debug_mode c.debug;
  Errors.color := not c.no_color;
  List.iter Files_legacy.add_path c.load_path_legacy

(** [quiet c] returns [true] if quiet mode has been activated. *)
let quiet (c : t) : bool = c.debug = "q"

let load_path (c : t) : Files.t = c.load_path
