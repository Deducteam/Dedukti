open Types
open Lua_api

let out                 = ref stdout
let name                = ref "anon"
let generate_lua_file   = ref false
let do_not_check        = ref false
let quiet               = ref false
let ignore_redefinition = ref false
let libs  :  string list ref       = ref []
let state : (Lua.state option ref) = ref None


let debug str  = if !quiet then () else ( prerr_string str ; flush stderr )
let debug_ok _ = if !quiet then () else prerr_endline "\027[32m[OK]\027[m"
let debug_ko _ = if !quiet then () else prerr_endline "\027[31m[KO]\027[m"


(* true->local , false->global *)             
let gs : (id,bool) Hashtbl.t = Hashtbl.create 47

let mk_var (id,loc) =
  try 
    if Hashtbl.find gs id then Var id
    else GVar id
  with
    | Not_found -> raise (ParsingError (ScopeError (id,loc))) 

let mk_evar qid = EVar qid      (*FIXME (qid = mod.id) vérifier que mod <> !name*)

let filter_qid qid = qid        (*FIXME idem *)

let mk_pat_var (id,loc) =
  try
    if Hashtbl.find gs id then Id id
    else Pat (id,[||],[||])
  with
    | Not_found -> raise (ParsingError (ScopeError (id,loc))) 

let gscope_add (id,loc) = 
  if Hashtbl.mem gs id then raise (ParsingError (AlreadyDefinedId (id,loc))) 
  else Hashtbl.add gs id false

let gscope_add_decl (id,loc) = 
  if Hashtbl.mem gs id then false
  else ( Hashtbl.add gs id false ; true )

let chk_rules_id (loc,(id,_)) = 
  if Hashtbl.mem gs id then () 
  else raise (ParsingError (ScopeError (id,loc)))

let lscope_add id = Hashtbl.add gs id true

let lscope_remove id = Hashtbl.remove gs id

let lscope_remove_lst lst = List.iter (fun x -> lscope_remove (fst x)) lst
