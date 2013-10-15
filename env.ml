
open Types

(* *** Environment management *** *)

type gst =
  | Decl  of term*(int*gdt) option 
  | Def   of term*term

let envs : (gst StringH.t) StringH.t = StringH.create 19 

let init name = StringH.add envs name (StringH.create 251)

(* *** Modules *** *)

let import m =
  if StringH.mem envs m then raise (EnvError ("Already opened module '"^m^"'."))
  else
    try 
      let chan = open_in (m^".dko") in
      let ctx:gst StringH.t = Marshal.from_channel chan in
        StringH.add envs m ctx
    with _ -> raise (EnvError ("Fail to open module '"^m^"'."))

let export_and_clear () = 
  ( if !Global.export then
    let out = open_out (!Global.name^".dko") in 
    let env = StringH.find envs !Global.name in
      Marshal.to_channel out env [Marshal.Closures] ) ;
  StringH.clear envs 

(* *** Get *** *)

let get_global_symbol m v = 
  let env = 
    try StringH.find envs m 
    with Not_found -> ( import m ; StringH.find envs m )
  in
    try ( StringH.find env v )
    with Not_found -> 
      raise (EnvError ("Cannot find symbol '"^m^"."^v^"'.")) 

let get_global_type m v = 
  match get_global_symbol m v with
    | Decl (ty,_)       -> ty
    | Def (_,ty)        -> ty 

let get_global_rw m v = 
  match get_global_symbol m v with
    | Decl (_,rw)       -> rw
    | Def (_,_)         -> None

(* *** Add *** *)

let add_decl v ty = 
  let env = StringH.find envs !Global.name in
    if StringH.mem env v then 
      if !Global.raphael then
        Global.print_v "Warning: Redeclaration (ignored).\n"
      else 
        raise (EnvError ("Already defined symbol '"^v^"'.")) 
    else 
      StringH.add env v (Decl (ty,None))

let add_def v te ty =
  let env = StringH.find envs !Global.name in
    if StringH.mem env v then 
      if !Global.raphael then
        Global.print_v "Redeclaration ignored.\n"
      else
        raise (EnvError ("Already defined symbol '"^v^"'."))
    else 
      StringH.add env v (Def (te,ty))

let add_rw (v:string) (g:int*gdt) = 
  let env = StringH.find envs !Global.name in
    try (
      match StringH.find env v with
        | Def (_,_)             -> raise (EnvError "Cannot rewrite a defined symbol.")
        | Decl(_,Some _)        -> raise (EnvError ("Cannot add rewrite rules for '"^v^"'."))
        | Decl (ty,None)        -> StringH.add env v (Decl (ty,Some g))
    ) with
      Not_found -> 
        raise (EnvError ("Cannot find symbol '"^(!Global.name)^"."^v^"'.")) 


