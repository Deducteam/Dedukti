open Cmdliner

let default_t = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ Config.t))

let default_i =
  let doc = "An implementation of the lambdaPi calculus modulo rewriting" in
  let sdocs = Manpage.s_common_options in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Dedukti is a set of tools to interact with files written in the \
         Dedukti language.";
    ]
  in
  let version = Version.version in
  Cmd.info "dk check" ~version ~doc ~sdocs ~man

let cmds =
  [Dkcheck.cmd; Dkdep.cmd; Dkpretty.cmd; Dkprune.cmd; Dkmeta.cmd; Dktop.cmd]

let () = exit Cmd.(eval @@ group ~default:default_t default_i cmds)
