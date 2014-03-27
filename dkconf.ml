open Types

let export = ref None

let args = [
 ( "--export", Arg.String (fun s -> export := Some (open_in s)),
                                                "Export in the TPDB format")
]

let main dko =
  Confluence.init dko ;
  ( match !export with None -> () | Some out -> Confluence.export out ) ;

  let confluence                =
    match !Confluence.weak_confluence, !Confluence.linearity with
      | WCR_No, _              ->
          begin
            (* BR is not weakly confluent hence not confluent *)
            Global.print "Confluence: NO.";
            Global.print "The rewrite system is not weakly confluent.";
            false
          (*TODO give counterexample *)
          end
      | (WCR_R|WCR_BR), true    ->
          begin
            (* BR is orthogonal hence confluent *)
            Global.print "Confluence: YES.";
            Global.print "The rewrite system is orthogonal.";
            true
          end
      | WCR_R, false            ->
          begin
            if !Confluence.modularity then (
              Global.print "Confluence: MAYBE.";
              Global.print "Modularity of Confluence: YES.";
              Global.print "You should use the export\
                functionnality to check that the rewrite system without beta is confluent."
            ) else (
              Global.print "Confluence: MAYBE.";
              Global.print "The rewrite system is weakly\
                confluent without beta but no modularity criterion holds.";
            ) ; false
          end
      | WCR_BR, false           ->
          begin
            (* Cannot prove confluence by orthogonality (not linear)
             * Cannot prove confluence by modularity (not R-WCR) *)
            Global.print "Confluence: MAYBE.";
            Global.print "The rewrite system is weakly confluent but only with beta.";
            false
          end
  in
    if confluence || !Confluence.type_level_applicative then
      Global.print "Subject Reduction: YES."
    else
      Global.print "Subject Reduction: MAYBE."

let _ =
  try (
    Arg.parse args main ("Usage: "^ Sys.argv.(0) ^" [options] file.dko");
    ( match !export with None -> () | Some input -> close_in input )
  ) with
      Sys_error err -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
