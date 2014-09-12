open Types

let color = ref true

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m"
  else s

let green = colored 2
let orange = colored 3
let red = colored 1

let success fmt =
  prerr_string (green "SUCCESS ") ;
  Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt

let prerr_loc lc =
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c

let debug fmt =
  Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt

let fail lc fmt =
  prerr_string (red "ERROR ") ;
  prerr_loc lc;
  Printf.kfprintf (fun _ -> prerr_newline () ; raise Exit) stderr fmt
