open Types

let name                = ref empty
let run_on_stdin        = ref false
let file                = ref "no_file"
let version             = "2.2.1"

let debug_level         = ref 0
let out                 = ref stdout
let export              = ref false
let ignore_redecl       = ref false
let color               = ref true
let autodep             = ref false

let print fmt = Printf.kfprintf (fun _ -> print_newline () ) stdout fmt
let print_out fmt = Printf.kfprintf (fun _ -> output_string !out "\n" ) !out fmt

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m"
  else s
let green = colored 2
(*let orange = colored 3*)
let red = colored 1

let success fmt = 
  prerr_string (green "SUCCESS ") ;
  Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt

let prerr_loc lc = 
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c

let debug lvl lc fmt = 
  if lvl <= !debug_level then ( 
    prerr_loc lc ;  
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt )
  else Printf.ifprintf stderr fmt

let debug_no_loc lvl fmt = 
  if lvl <= !debug_level then
    Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt
  else Printf.ifprintf stderr fmt

let fail lc fmt = 
  prerr_string (red "ERROR ") ;
  prerr_loc lc;
  Printf.kfprintf (fun _ -> prerr_newline () ; raise Exit) stderr fmt
