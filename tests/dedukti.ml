module Command = struct
  type t = Dkcheck | Dkmeta

  let path = function
    | Dkcheck -> "./dkcheck.native"
    | Dkmeta  -> "./dkmeta.native"
end

let run ?(preprocess = return) ~regression ~error ~title ~tags ~filename command
    arguments =
  let register f =
    match regression with
    | None             -> Test.register ~__FILE__:filename ~title ~tags f
    | Some output_file ->
        Regression.register ~__FILE__:filename ~output_file
          ~regression_output_path:"tests/tezt/_regressions" ~title ~tags f
  in
  register (fun () ->
      let* () = preprocess () in
      let output_options =
        (* --no-color: Do not print color characters in regression output. *)
        if regression <> None then ["--no-color"; "-q"]
        else if Cli.options.log_level = Cli.Report then ["-q"]
        else if Cli.options.log_level = Cli.Debug then ["-d"; "montru"]
        else if Cli.options.log_level = Cli.Info then ["-d"; "n"]
        else []
      in
      let arguments = output_options @ arguments in
      (* In [Info] mode, we have to tell to Tezt which lines need to
         be reported. By default, in [Debug] mode, Tezt will report
         all the logged lines. Nothing should be printed in [Report]
         mode. *)
      let hooks =
        if regression <> None then Some Regression.hooks
        else if Cli.options.log_level = Cli.Info then
          Some
            Process.
              {
                on_log = (fun line -> Log.info "%s" line);
                on_spawn =
                  (fun command arguments ->
                    let message = Log.quote_shell_command command arguments in
                    Log.info ~color:Log.Color.bold ~prefix:command "%s" message);
              }
        else None
      in
      let command = Command.path command in
      let process = Process.spawn ?hooks command (arguments @ [filename]) in
      match error with
      | None              -> Process.check process
      | Some (`Code code) ->
          (* We check the process returned on [stderr] the expected error code *)
          Process.check_error
            ~msg:Base.(rex (sf "[ERROR CODE:%d]" code))
            process
      | Some `System      ->
          Process.check_error ~msg:(Base.rex "[ERROR CODE:SYSTEM]") process)

let title ~action ~result ~options filename =
  let options_str = String.concat ", " options in
  let basename = Filename.basename filename in
  match options with
  | []       -> Format.asprintf "%s '%s' %s" action basename result
  | [option] ->
      Format.asprintf "%s '%s' %s with '%s'" action basename result option
  | _ :: _   ->
      Format.asprintf "%s '%s' %s with '%s'" action basename result options_str

module Check = struct
  type argument = Eta | Import of string

  let mk_argument = function Eta -> ["--eta"] | Import path -> ["-I"; path]

  let tag_of_argument = function Eta -> "eta" | Import _ -> "import"

  let run ~regression ~error ~filename arguments =
    let tags = List.map tag_of_argument arguments in
    let arguments = List.map mk_argument arguments |> List.concat in
    let result = if error <> None then "fails" else "succeeds" in
    let result_tag = if error <> None then "ko" else "ok" in
    let title = title ~action:"check" ~result ~options:tags filename in
    let tags = "dkcheck" :: result_tag :: tags in
    let regression =
      if regression then Some (String.concat "_" (filename :: tags)) else None
    in
    run ~regression ~error ~title ~tags ~filename Dkcheck arguments

  let ok ?(regression = false) = run ~regression ~error:None

  let ko ~error = run ~regression:false ~error:(Some error)

  let export filename = Process.run Command.(path Dkcheck) ["-e"; filename]
end

module Meta = struct
  type argument = No_meta | No_beta | Meta of string | Import of string

  let mk_argument = function
    | No_meta     -> ["--no-meta"]
    | No_beta     -> ["--no-beta"]
    | Meta file   -> ["-m"; file]
    | Import path -> ["-I"; path]

  let tag_of_argument = function
    | No_meta      -> "no_meta"
    | No_beta      -> "no_beta"
    | Meta _file   -> "meta"
    | Import _path -> "import"

  let run ?(dep = []) ~filename arguments =
    let tags = List.map tag_of_argument arguments in
    let arguments = List.map mk_argument arguments |> List.concat in
    let title =
      title ~action:"metaify" ~result:"succeeds" ~options:tags filename
    in
    let tags = "dkmeta" :: tags in
    let regression = Some (String.concat "_" (filename :: tags)) in
    let preprocess () = Lwt_list.iter_s Check.export dep in
    let import_arguments =
      List.map (fun dir -> ["-I"; Filename.dirname dir]) dep |> List.concat
    in
    run ~regression ~error:None ~title ~tags ~filename Dkmeta
      (import_arguments @ arguments)
      ~preprocess
end
