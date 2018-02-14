open Term

let default_mident = ref None

let set_default_mident md = default_mident := Some md

let run_on_file file =
  if !(Dep.verbose) then Printf.eprintf "Running Dkdep on file \"%s\".\n" file;
  flush stderr;
  let input = open_in file in
  Dep.filename := file;
  let md =  Basic.mk_mident (match !default_mident with None -> file | Some str -> str) in
  Dep.init md;
  Parser.handle_channel md Checker.mk_entry input;
  Dep.finalize ();
  close_in input


let args =
  [ ("-o", Arg.String (fun fi -> Dep.out := open_out fi), "Output file"  );
    ("-v", Arg.Set Dep.verbose, "Verbose mode" );
    ("-s", Arg.Set Dep.sorted, "Sort file with respect to their dependence");
    ("-I", Arg.String Basic.add_path, "Add a directory to load path, dependencies to files found in load path are not printed");
    ("-module" , Arg.String set_default_mident     , "Give a default name to the current module");
  ]

let print_out fmt = Printf.kfprintf (fun _ -> output_string stdout "\n" ) stdout fmt

let _ =
  try
    Arg.parse args run_on_file "Usage: dkdep [options] files";
    if !Dep.sorted then
      let l = Dep.sort() in
      print_out "%s" (String.concat " " l)
  with
    | Sys_error err             -> Printf.eprintf "ERROR %s.\n" err; exit 1
    | Exit                      -> exit 3
