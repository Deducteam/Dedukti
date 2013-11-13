
open Types

(* *** Global Options *** *)

let name                        = ref empty
let quiet                       = ref true
let export                      = ref false
let raphael                     = ref false
let color                       = ref true
let out                         = ref stdout (* for dk2mmt *)
let filename                    = ref None

let set_name s = 
  name := hstring s

let set_out file =
  out := open_out file

let set_file_name s =
  filename := Some s

(* *** Info messages *** *)

let sprint = print_endline
let eprint = prerr_endline
let vprint str  = if not !quiet then prerr_endline (Lazy.force str)

let colored n s =
  if !color
  then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m"
  else s

let green = colored 2
let orange = colored 3
let red = colored 1

let print_ok _ = vprint (lazy (green "[DONE]"))

let string_of_lc lc =
  let file_str = match !filename with
      None -> ""
    | Some f -> " file:" ^ f
  in
  file_str
  ^ " line:" ^ string_of_int (get_line lc)
  ^ " column:" ^ string_of_int (get_column lc)
  ^ " "

let warning lc str =
  let w' = orange "WARNING" in
    eprint ( w' ^ string_of_lc lc ^ str )

let error lc str =
  let e' = red "ERROR" in
    eprint ( e' ^ string_of_lc lc ^ str ) ; 
    exit 1 
