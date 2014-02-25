
open Types

(* *** Global Options *** *)

let name                        = ref empty
let quiet                       = ref true
let export                      = ref false
let raphael                     = ref false
let color                       = ref true
let out                         = ref stdout (* for dk2mmt *)
let filename                    = ref None
let tpdb                        = ref false
let tpdb_file                   = ref stderr (*dummy instantiation*)

let set_name s =
  name := s

let set_out file =
  out := open_out file

let set_filename s =
  filename := Some s

let set_tpdb file =
  let out = open_out file in
    tpdb := true ;
    tpdb_file := stdout ; (*FIXME*)
    Printf.fprintf stdout "(VAR _ #DB0 #DB1 #DB2 #DB3 #DB4 #DB5 #DB6 #DB7 #DB8 #DB9)\n" 

(* *** Info messages *** *)

let string_of_loc lc =
  match !filename with
    | None      -> string_of_loc lc
    | Some fn   -> "file:" ^ fn ^ " " ^ string_of_loc lc 

let sprint = print_endline
let eprint = prerr_endline

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m"
  else s

let green = colored 2
let orange = colored 3
let red = colored 1

let vprint lc str  =
  if not !quiet then prerr_endline ( "[" ^ string_of_loc lc ^ "] " ^ Lazy.force str )

let print_ok _ =
  match !filename with
    | None      -> eprint (green "Checked")
    | Some fn   -> eprint ("File " ^ fn ^ " was successfully " ^ green "Checked" ^ ".")

let warning lc str =
  eprint ( orange "WARNING " ^ string_of_loc lc ^ " " ^ str )

let error lc str =
  eprint ( red "ERROR " ^ string_of_loc lc ^ " " ^ str ) ;
  exit 1

(* Profiling *)

let nbConvTest = ref 0
let incr_NbConvTest _ = incr nbConvTest

let nbTrivialTest = ref 0
let incr_NbTrivialTest _ = incr nbTrivialTest

let print_stats _ =
  let nb_conv = float !nbConvTest in
  let nb_eq   = float !nbTrivialTest in
  let rate0 = ( nb_eq /. nb_conv ) *. 100.0 in
  let rate  = ( floor ( rate0 *. 10.0 ) ) /. 10.0 in
    eprint ("Conversion tests: " ^ string_of_int !nbConvTest);
    eprint ("Trivial conversion tests: " ^ string_of_int !nbTrivialTest ^ " ("^string_of_float rate ^"%)")

