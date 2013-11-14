
open Types

(* *** Global Options *** *)

let name                        = ref empty
let filename                    = ref "none"
let quiet                       = ref true
let export                      = ref false
let raphael                     = ref false
let color                       = ref true
let out                         = ref stdout (* for dk2mmt *)

let set_name s =
  name := s

let set_out file =
  out := open_out file

let set_filename fn =
  filename := fn

(* *** Info messages *** *)

let sprint = print_endline
let eprint = prerr_endline

let vprint lc str  =
  if not !quiet then prerr_endline ( (Lazy.force str) ^ " " ^ (string_of_loc lc) )

let print_ok _ =
  if !color then eprint ("\027[32mDONE\027[m file "^ (!filename))
  else eprint ("DONE file "^ (!filename))

let warning lc str =
  let w' = if !color then "\027[33mWARNING\027[m" else "WARNING" in
    eprint ( w' ^ " file:" ^ !filename ^ " " ^ string_of_loc lc ^ " " ^ str )

let error lc str =
  let e' = if !color then "\027[31mERROR\027[m" else "ERROR" in
    eprint ( e' ^ " file:" ^ !filename ^ " " ^ string_of_loc lc ^ " " ^ str ) ;
    exit 1
