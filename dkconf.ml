open Types
open Confluence

let export = ref None

let args = [
 ( "--export", Arg.String (fun s -> export := Some (open_out s)),
                                                "Export in the TPDB format")
]

let pp_bool out b =
  if b then Printf.fprintf out "YES"
  else Printf.fprintf out "NO"

let pp_bool_yes_maybe out b =
  if b then Printf.fprintf out "YES"
  else Printf.fprintf out "MAYBE"

let pp_wcr out = function
  | WCR_R  -> Printf.fprintf out "YES (without beta)"
  | WCR_BR -> Printf.fprintf out "YES"
  | WCR_No -> Printf.fprintf out "NO"

let main dko =
  init dko ;
  (*FIXME print stats*)
  Global.print "Linearity: %a." pp_bool !linearity;
  Global.print "Local Confluence: %a." pp_wcr !weak_confluence;
  ( if !linearity && !toyama_criterion then
      Global.print "Modularity of Confluence: YES \
                                    (by linearity and Toyama's criterion)."
    else if !linearity then
      Global.print "Modularity of Confluence: YES (by linearity)."
    else if !toyama_criterion then
      Global.print "Modularity of Confluence: YES (by Toyama's criterion)."
    else
      Global.print "Modularity of Confluence: MAYBE."
  ) ;
  match !weak_confluence with
    | (WCR_R|WCR_BR) when !linearity ->
        Global.print "Confluence: YES (by orthogonality)."
    | WCR_No ->
        begin
          Global.print "Confluence: NO.";
          (*TODO give counterexample *)
          Global.print "Subject Reduction: %a." pp_bool_yes_maybe
            !subject_reduction_criterion;
        end
    | _ ->
        begin
          Global.print "Confluence: MAYBE.";
          Global.print "Subject Reduction: %a." pp_bool_yes_maybe
            !subject_reduction_criterion;
        end

  let _ =
  try (
    Arg.parse args main ("Usage: "^ Sys.argv.(0) ^" [options] file.dko");
    ( match !export with None -> () | Some file -> close_out file )
  ) with
      Sys_error err -> ( Printf.eprintf "ERROR %s.\n" err; exit 1 )
