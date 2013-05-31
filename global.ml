open Types

(* Options *)

let action                      = ref LuaGeneration
let out                         = ref stdout
let name                        = ref ""
let lua_path                    = ref ""
let do_not_check                = ref false
let check_ext                   = ref true
let quiet                       = ref false
let ignore_redeclarations       = ref false
let libs : string list ref      = ref []

(* Global functions *) 

let set_mmt _ =
  action := PrintMMT

let set_out file =
  try out   := open_out file
  with Sys_error err -> raise (OptionError (SetOutError (file,err)))

let set_path path =
  try 
    if Sys.is_directory path then lua_path := path 
    else raise (OptionError (SetLuaPathError (path,"not a directory")))
  with Sys_error err ->
    raise (OptionError (SetLuaPathError (path,err)))

let set_name (l,str) =
  if Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z_0-9]*") str 0 then name := str 
  else raise (ParserError (SetNameError (str,l)))

let add_lib s = libs := s::(!libs)

(* Debug *)

let debug str  = if !quiet then () else ( prerr_string str ; flush stderr )
let debug_ok _ = if !quiet then () else prerr_endline "\027[32m[OK]\027[m"
let debug_ig _ = if !quiet then () else prerr_endline "\027[33m[IGNORED]\027[m"
let debug_ko _ = if !quiet then () else prerr_endline "\027[31m[KO]\027[m"

