open Types

let name                = ref empty
let run_on_stdin        = ref false
let file                = ref "no_file"
let version             = "2.2.1"

let debug_level         = ref 0
let out                 = ref Format.std_formatter
let export              = ref false
let ignore_redecl       = ref false
let color               = ref true
let autodep             = ref false

let print fmt = Format.kfprintf (fun _ -> Format.print_newline () ) Format.std_formatter fmt
let print_out fmt = Format.kfprintf (fun _ -> Format.pp_print_newline !out ()) !out fmt

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m"
  else s
let green = colored 2
(*let orange = colored 3*)
let red = colored 1

let err = Format.err_formatter

let success fmt =
  Format.pp_print_string err (green "SUCCESS ") ;
  Format.kfprintf (fun out -> Format.pp_print_newline out () ) err fmt

let prerr_loc lc =
  let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c

let debug lvl lc fmt =
  if lvl <= !debug_level then (
    prerr_loc lc ;
    Format.kfprintf (fun out -> Format.pp_print_newline out () ) err fmt )
  else Format.ifprintf err fmt

let debug_no_loc lvl fmt =
  if lvl <= !debug_level then
    Format.kfprintf (fun out -> Format.pp_print_newline out () ) err fmt
  else Format.ifprintf err fmt

let fail lc fmt =
  Format.pp_print_string err (red "ERROR ") ;
  prerr_loc lc;
  Format.kfprintf (fun out -> Format.pp_print_newline out () ; raise Exit) err fmt
