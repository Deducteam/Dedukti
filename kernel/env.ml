open Types
open Printf

let name = ref (hstring "noname")
let ignore_redecl = ref false
let autodep = ref false

let envs : (rw_infos H.t) H.t = H.create 19
let init nm =
  name := nm;
  H.add envs nm (H.create 251)

(******************************************************************************)

let import lc m =
  assert ( not (H.mem envs m) );
  (* If the [.dko] file is not found, try to compile it first.
   This hack is terrible. It uses system calls and can loop with circular dependencies. *)
  ( if !autodep && not ( Sys.file_exists ( string_of_ident m ^ ".dko" ) ) then
      if Sys.command ( "dkcheck -autodep -e " ^ string_of_ident m ^ ".dk" ) <> 0 then
        Print.fail lc "Fail to compile dependency '%a'." pp_ident m
  ) ;
    let (_,ctx) = Dko.unmarshal lc (string_of_ident m) in
      ( H.add envs m ctx ; ctx )

let get_deps () : string list =
  H.fold (
    fun md _ lst ->
      if ident_eq md !name then lst
      else (string_of_ident md)::lst
  ) envs []

let clear () = H.clear envs
let export () = Dko.marshal !name (get_deps ()) (H.find envs !name)

(******************************************************************************)

let get_infos lc m v =
  let env =
    try H.find envs m
    with Not_found -> import lc m
  in
    try ( H.find env v )
    with Not_found ->
      Print.fail lc "Cannot find symbol '%a.%a'." pp_ident m pp_ident v

let get_type lc m v =
  match get_infos lc m v with
    | Decl ty
    | Def (_,ty)
    | Decl_rw (ty,_,_,_) -> ty

(******************************************************************************)

let add lc v gst =
  let env = H.find envs !name in
    if H.mem env v then
      if !ignore_redecl then
        Print.debug "Redeclaration ignored."
      else
        Print.fail lc "Already defined symbol '%a'." pp_ident v
    else
      H.add env v gst

let add_decl lc v ty    = add lc v (Decl ty)
let add_def lc v te ty  = add lc v (Def (te,ty))

let add_rw = function
  | [] -> ()
  | r::_ as rs ->
      let env = H.find envs !name in
      let rwi = H.find env r.id in
        H.add env r.id (Dtree.add_rules rwi rs)
