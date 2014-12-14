type action = All | NonLinear | TypeLevel | Export
let action = ref All
let out = ref stdout

let args = [
  ("--list-all",        Arg.Unit (fun _ -> action := All),       "Print all rules" ) ;
  ("--list-non-linear", Arg.Unit (fun _ -> action := NonLinear), "Print non-linear rules" ) ;
  ("--list-type-level", Arg.Unit (fun _ -> action := TypeLevel), "Print type-level rules" ) ;
  ("--export",          Arg.Unit (fun _ -> action := Export),    "Export to TPDB format" ) ;
  ("-o", Arg.String (fun fi -> out := open_out fi), "Output file"  )
]

let flatten lst = List.flatten (List.map snd lst)

let run name = assert false (*FIXME*)
                 (*
  let rules = Env.get_all_rules name in
    match !action with
      | All -> Rules.print_all !out rules
      | NonLinear -> Rules.print_non_linear_rules !out rules
      | TypeLevel -> Rules.print_type_level_rules !out rules
      | Export -> Tpdb.export !out rules
                  *)
let _ =
  try Arg.parse args run ("Usage: "^ Sys.argv.(0) ^" [options] files");
  with Sys_error err -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
