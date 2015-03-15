let out = ref stdout

let args = [
    ("-o", Arg.String (fun fi -> out := open_out fi), "Output file"  )
]

let run name =
  let rs = Signature.get_all_rules name in
    Tpdb.export !out [(name,rs)]

let _ =
  try Arg.parse args run ("Usage: "^ Sys.argv.(0) ^" [options] files");
  with Sys_error err -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
