
open Types

type gst =
  | Decl  of term
  | Def   of term*term

let envs : (gst StringH.t) StringH.t = StringH.create 19 

let init name = StringH.add envs name (StringH.create 251)

(* Get *)

let get_global_symbol m v = 
  let env = 
    try StringH.find envs m 
    with Not_found -> raise (EnvError ("Cannot find module "^m))
  in
    try ( StringH.find env v )
    with Not_found -> 
      raise (EnvError ("Cannot find symbol '"^m^"."^v^"'")) 

let get_global_type m v = 
  match get_global_symbol m v with
    | Decl ty           -> ty
    | Def (_,ty)        -> ty 

let get_global_def m v = 
  match get_global_symbol m v with
    | Decl _          -> None
    | Def (te,_)      -> Some te 

(* Add *)

let add_decl v ty = 
  let env = StringH.find envs !Global.name in
    if StringH.mem env v then raise (EnvError ("Already defined symbol "^v)) 
    else StringH.add env v (Decl ty)

let add_def v te ty =
  let env = StringH.find envs !Global.name in
    if StringH.mem env v then raise (EnvError ("Already defined symbol "^v))
    else StringH.add env v (Def (te,ty))

(* Update*)
(* 
let update_def v te =
    match get_global_symbol v with
      | Decl _          -> assert false
      | Def (_,ty)      -> StringH.replace env v (Def (te,ty))
 *)

(* Modules *)

let import m =
  if StringH.mem envs m then raise (EnvError ("AlreadyOpenedModule "^m))
  else
    try 
      let chan = open_in (m^".dko") in
      let ctx:gst StringH.t = Marshal.from_channel chan in
        StringH.add envs m ctx
    with _ -> raise (EnvError ("FailToOpenModule "^m))

let export_and_clear () = 
  ( if !Global.export then
    let out = open_out (!Global.name^".dko") in 
    let env = StringH.find envs !Global.name in
      Marshal.to_channel out env [Marshal.Closures] ) ;
  StringH.clear envs 

(* Debug *)
(*
let dump_context h =
  StringH.iter (
  fun s d ->
    match d with
      | Def (_,_)       -> Global.msg (s^" is a def.\n")
      | Decl _          -> Global.msg (s^" is declared.\n")
  ) h

let dump_symbols () = 
  Global.msg ("### MODULE "^ !Global.name ^"\n");
  dump_context env; 
  StringH.iter (
    fun m h ->
      Global.msg ("### MODULE "^m^"\n");
      dump_context h
  ) ext *)
