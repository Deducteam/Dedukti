
open Types

(* *** Environment management *** *)

type gst =
  | Decl  of term*(int*gdt) option 
  | Def   of term*term

let envs : (gst StringH.t) StringH.t = StringH.create 19 

let init name = StringH.add envs name (StringH.create 251)

(* *** Modules *** *)

let import lc m =
  if StringH.mem envs m then raise (EnvError (lc,"Already opened module '"^m^"'."))
  else
    try 
      let chan = open_in (m^".dko") in
      let ctx:gst StringH.t = Marshal.from_channel chan in
        StringH.add envs m ctx
    with _ -> raise (EnvError (lc,"Fail to open module '"^m^"'."))

let export_and_clear () = 
  ( if !Global.export then
    let out = open_out (!Global.name^".dko") in 
    let env = StringH.find envs !Global.name in
      Marshal.to_channel out env [Marshal.Closures] ) ;
  StringH.clear envs 

(* *** Get *** *)

let get_global_symbol lc m v = 
  let env = 
    try StringH.find envs m 
    with Not_found -> ( import lc m ; StringH.find envs m )
  in
    try ( StringH.find env v )
    with Not_found -> 
      raise (EnvError (lc,"Cannot find symbol '"^m^"."^v^"'.")) 

let get_global_type lc m v = 
  match get_global_symbol lc m v with
    | Decl (ty,_)       -> ty
    | Def (_,ty)        -> ty 

let get_global_rw lc m v = 
  match get_global_symbol lc m v with
    | Decl (_,rw)       -> rw
    | Def (_,_)         -> None

(* *** Add *** *)

let add_decl lc v ty = 
  let env = StringH.find envs !Global.name in
    if StringH.mem env v then 
      if !Global.raphael then
        Global.vprint "Warning: Redeclaration (ignored).\n"
      else 
        raise (EnvError (lc,"Already defined symbol '"^v^"'.")) 
    else 
      StringH.add env v (Decl (ty,None))

let add_def lc v te ty =
  let env = StringH.find envs !Global.name in
    if StringH.mem env v then 
      if !Global.raphael then
        Global.vprint "Redeclaration ignored.\n"
      else
        raise (EnvError (lc,"Already defined symbol '"^v^"'."))
    else 
      StringH.add env v (Def (te,ty))

let add_rw lc (v:string) (g:int*gdt) = 
  let env = StringH.find envs !Global.name in
    try (
      match StringH.find env v with
        | Def (_,_)             -> raise (EnvError (lc,"Cannot rewrite a defined symbol."))
        | Decl(_,Some _)        -> failwith "Not implemented (re-add rewrite rules)." 
        | Decl (ty,None)        -> StringH.add env v (Decl (ty,Some g))
    ) with
      Not_found -> 
        raise (EnvError (lc,"Cannot find symbol '"^(!Global.name)^"."^v^"'.")) 


