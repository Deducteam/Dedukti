
open Types

type gst =
  | Decl  of term
  | Def   of term*term

module SHashtbl = Hashtbl.Make(struct type t = string let equal a b = a = b let hash = Hashtbl.hash end)
type env = gst SHashtbl.t

let env : env = SHashtbl.create 251
let ext : env SHashtbl.t = SHashtbl.create 19 

(* Get *)

let get_symbol v =
  try ( SHashtbl.find env v )
  with Not_found -> 
    raise (TypingError (UndefinedSymbol (!Global.name,v)))

let get_ext_symbol m v =
  let h =
    try SHashtbl.find ext m 
    with Not_found -> failwith "Could not find the module" 
  in
    try ( SHashtbl.find h v )
    with Not_found -> 
      raise (TypingError (UndefinedSymbol (m,v)))

type get_type_type =
  | HTerm of term
  | NotHTerm of term

let get_type (m,v) = 
  if m == !Global.name then
    ( match get_symbol v with
      | Decl ty         -> HTerm ty
      | Def (_,ty)      -> HTerm ty )
  else
    ( match get_ext_symbol m v with
        | Decl ty       -> NotHTerm ty
        | Def (_,ty)    -> NotHTerm ty )

type get_def_type =
  | NoNe
  | SoMe of term
  | Ext of term

let get_def (m,v) = 
  if m == !Global.name then
    ( match get_symbol v with
      | Decl _          -> NoNe
      | Def (te,_)      -> SoMe te )
  else
    ( match get_ext_symbol m v with
        | Decl _        -> NoNe
        | Def (te,_)    -> Ext te )

(* Add *)

let add_decl (v,ty) = 
  if SHashtbl.mem env v then failwith "Already defined id" 
  else SHashtbl.add env v (Decl ty)

let add_def (v,te,ty) =
  if SHashtbl.mem env v then failwith "Already defined id"
  else SHashtbl.add env v (Def (te,ty))

(* Modules *)

let import m =
  if SHashtbl.mem ext m then failwith "import (1)"
  else
    try 
      let chan = open_in (m^".dko") in
      let ctx:env = Marshal.from_channel chan in
        SHashtbl.add ext m ctx
    with Not_found -> failwith "import (2)"

let export_and_clear () = 
  ( if !Global.export then
    let out = open_out (!Global.name^".dko") in (*FIXME*)
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
