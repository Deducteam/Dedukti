
open Types

(* *** Global Options *** *)

let name                        = ref empty
let quiet                       = ref true
let export                      = ref false
let raphael                     = ref false
let color                       = ref true
let out                         = ref stdout (* for dk2mmt *)

let set_name s = 
  name := hstring s

let set_out file =
  out := open_out file

(* *** Info messages *** *)

let sprint = print_endline
let eprint = prerr_endline
let vprint str  = if not !quiet then prerr_endline (Lazy.force str)

let print_ok filename =                       
  if !color then eprint ("\027[32mDONE\027[m file "^filename)
  else eprint ("DONE file "^filename)

let warning lc str = 
  let w' = if !color then "\027[33mWARNING\027[m" else "WARNING" in
    eprint ( w' ^ " line:" ^ string_of_int (get_line lc) ^ " column:" 
             ^ string_of_int (get_column lc) ^ " " ^ str )

let error lc str = 
  let e' = if !color then "\027[31mERROR\027[m" else "ERROR" in
    eprint ( e' ^ " line:" ^ string_of_int (get_line lc) ^ " column:" 
             ^ string_of_int (get_column lc) ^ " " ^ str ) ; 
    exit 1 
