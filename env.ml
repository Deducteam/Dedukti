
open Types

module H = Hashtbl.Make(
struct 
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash 
end ) 

(* *** Environment management *** *)

type gst =
  | Decl  of term*(int*gdt) option 
  | Def   of term*term

let envs : (gst H.t) H.t = H.create 19 

let init name = H.add envs name (H.create 251)

(* *** Modules *** *)

let import lc m =
  if H.mem envs m then raise (EnvError ( lc , "Already opened module '" ^ string_of_ident m ^ "'." ) )
  else
    try 
      let chan = open_in ( string_of_ident m ^ ".dko" ) in
      let ctx:gst H.t = Marshal.from_channel chan in
        close_in chan ;
        H.add envs m ctx
    with _ -> raise (EnvError ( lc , "Fail to open module '" ^ string_of_ident m ^ "'." ) )

let export_and_clear () = 
  ( if !Global.export then
      begin
        let out = open_out (string_of_ident !Global.name ^ ".dko" ) in 
        let env = H.find envs !Global.name in
          Marshal.to_channel out env [Marshal.Closures] ;
          close_out out 
      end ) ;
  H.clear envs 

(* *** Get *** *)

let get_global_symbol lc m v = 
  let env = 
    try H.find envs m 
    with Not_found -> ( import lc m ; H.find envs m ) (*TODO*)
  in
    try ( H.find env v )
    with Not_found -> 
      raise (EnvError ( lc , "Cannot find symbol '" ^ string_of_ident m ^ "." ^ string_of_ident v ^ "'." ) ) 

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
  let env = H.find envs !Global.name in
    if H.mem env v then 
      if !Global.raphael then
        Global.vprint (lazy "Warning: Redeclaration (ignored).\n")
      else 
        raise (EnvError ( lc , "Already defined symbol '" ^string_of_ident v ^ "'." )) 
    else 
      H.add env v (Decl (ty,None))

let add_def lc v te ty =
  let env = H.find envs !Global.name in
    if H.mem env v then 
      if !Global.raphael then
        Global.vprint (lazy "Redeclaration ignored.\n")
      else
        raise (EnvError ( lc , "Already defined symbol '" ^ string_of_ident v ^ "'." ))
    else 
      H.add env v (Def (te,ty))

let add_rw lc v g = 
  let env = H.find envs !Global.name in
    try (
      match H.find env v with
        | Def (_,_)             -> raise (EnvError (lc,"Cannot rewrite a defined symbol."))
        | Decl(_,Some _)        -> failwith "Not implemented (re-add rewrite rules)." 
        | Decl (ty,None)        -> H.add env v (Decl (ty,Some g))
    ) with
      Not_found -> 
        raise (EnvError ( lc , "Cannot find symbol '" ^ string_of_ident !Global.name ^ "." ^ string_of_ident v ^ "'." )) 


