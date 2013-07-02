
open Types

type gst =
  | Decl  of term
  | Def   of term*term

type env = gst SHashtbl.t

let env : env = SHashtbl.create 251
let ext : env SHashtbl.t = SHashtbl.create 19 

(* Get *)

let get_env m = 
  if m = !Global.name then env
  else
    ( try SHashtbl.find ext m 
      with Not_found -> raise (EnvError (CannotFindModule m)))

let get_symbol (m,v) =
  let ht = get_env m in
  try ( SHashtbl.find ht v )
  with Not_found -> raise (EnvError (UndefinedSymbol (m,v)))

let get_type id = 
  match get_symbol id with
    | Decl ty           -> ty
    | Def (_,ty)        -> ty 

let get_def id = 
  match get_symbol id with
    | Decl _          -> None
    | Def (te,_)      -> Some te 

(* Add *)

let add_decl (v,ty) = 
  if SHashtbl.mem env v then raise (EnvError (AlreadyDefinedSymbol v)) 
  else SHashtbl.add env v (Decl ty)

let add_def (v,te,ty) =
  if SHashtbl.mem env v then raise (EnvError (AlreadyDefinedSymbol v))
  else SHashtbl.add env v (Def (te,ty))

(* Modules *)

let import m =
  if SHashtbl.mem ext m then raise (EnvError (AlreadyOpenedModule m))
  else
    try 
      let chan = open_in (m^".dko") in
      let ctx:env = Marshal.from_channel chan in
        SHashtbl.add ext m ctx
    with _ -> raise (EnvError (FailToOpenModule m))

let export_and_clear () = 
  ( if !Global.export then
    let out = open_out (!Global.name^".dko") in 
      Marshal.to_channel out env [Marshal.Closures] ) ;
  SHashtbl.clear env ;
  SHashtbl.clear ext

(* Debug *)

let dump_context h =
  SHashtbl.iter (
  fun s d ->
    match d with
      | Def (_,_)       -> Global.msg (s^" is a def.\n")
      | Decl _          -> Global.msg (s^" is declared.\n")
  ) h

let dump_symbols () = 
  Global.msg ("### MODULE "^ !Global.name ^"\n");
  dump_context env; 
  SHashtbl.iter (
    fun m h ->
      Global.msg ("### MODULE "^m^"\n");
      dump_context h
  ) ext
