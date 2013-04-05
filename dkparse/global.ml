open Types

(* Options *)

let out                         = ref stdout
let name                        = ref ""
let lua_path                    = ref ""
let do_not_check                = ref false
let check_ext                   = ref true
let quiet                       = ref false
let ignore_redeclarations       = ref false
let libs : string list ref      = ref []

let set_out file =
  try out   := open_out file
  with Sys_error err -> raise (OptionError (SetOutError (file,err)))

let set_path path =
  try 
    if Sys.is_directory path then lua_path := path 
    else raise (OptionError (SetLuaPathError (path,"not a directory")))
  with Sys_error err ->
    raise (OptionError (SetLuaPathError (path,err)))

let set_name str =
  if Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z_0-9]*") str 0 then name := str 
  else raise (OptionError (SetNameError str))

let add_lib s = libs := s::(!libs)

(* Debug *)

let debug str  = if !quiet then () else ( prerr_string str ; flush stderr )
let debug_ok _ = if !quiet then () else prerr_endline "\027[32m[OK]\027[m"
let debug_ko _ = if !quiet then () else prerr_endline "\027[31m[KO]\027[m"

(* Check for external symbols *)

let mchecked : (string list) ref = ref []

let is_checked m =
  if List.mem m !mchecked then false
  else ( mchecked := m::!mchecked ; true )

(* Term construction & Scoping *)

type vart = Local | Global

let gs : (string,vart) Hashtbl.t = Hashtbl.create 47

let mk_var (id,loc) =
  try
   begin
     match Hashtbl.find gs id with 
       | Local   -> Var id
       | _       -> GVar (!name,id)
   end
  with
    | Not_found -> raise (ParserError (ScopeError ((!name,id),loc))) 

let mk_evar (m,v,l) = 
  if m = !name then 
    begin
      try
        begin
          match Hashtbl.find gs v with 
            | Local   -> raise (ParserError (ScopeError ((m,v),l)))
            | _       -> GVar (m,v)
        end
      with
        | Not_found -> raise (ParserError (ScopeError ((m,v),l)))
    end
  else 
      (*if List.mem m !libs then *)
        GVar (m,v)
      (*else raise (ParsingError (UnknownModule (m,l))) *)

let filter_qid (md,id,l) = 
  if md = !name then 
    begin
      try
        begin
          match Hashtbl.find gs id with 
            | Local   -> raise (ParserError (ScopeError ((md,id),l)))
            | _       -> (md,id)
        end
      with
        | Not_found -> raise (ParserError (ScopeError ((md,id),l)))
    end
  else  
      (*if List.mem md !libs then *)
        (md,id)
      (*else raise (ParsingError (UnknownModule (md,l)))*)

let mk_pat_var (id,loc) =
  try
    match Hashtbl.find gs id with
      | Local -> Id id
      | _     -> Pat ((!name,id),[||],[||])
  with
    | Not_found -> raise (ParserError (ScopeError ((!name,id),loc))) 

let gscope_add (id,loc) = 
  if Hashtbl.mem gs id then raise (ParserError (AlreadyDefinedId ((!name,id),loc))) 
  else 
    Hashtbl.add gs id Global 

let gscope_add_decl (id,loc) = 
  if Hashtbl.mem gs id then false
  else ( 
    Hashtbl.add gs id Global ; 
    true 
  )

let chk_rules_id (loc,(id,_)) = 
  if Hashtbl.mem gs id then ()
  else raise (ParserError (ScopeError ((!name,id),loc)))

let lscope_add id = Hashtbl.add gs id Local

let lscope_remove id = Hashtbl.remove gs id

let lscope_remove_lst lst = List.iter (fun x -> lscope_remove (fst (fst x))) lst
