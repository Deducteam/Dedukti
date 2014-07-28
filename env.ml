open Types
open Printf

let envs : (rw_infos H.t) H.t = H.create 19
let init name = H.add envs name (H.create 251)

(******************************************************************************)

let import lc m =
  assert ( not (H.mem envs m) );
  (* If the [.dko] file is not found, try to compile it first.
   This hack is terrible. It uses system calls and can loop with circular dependencies. *)
  ( if !Global.autodep && not ( Sys.file_exists ( string_of_ident m ^ ".dko" ) ) then
      if Sys.command ( "dkcheck -autodep -e " ^ string_of_ident m ^ ".dk" ) <> 0 then
        Global.fail lc "Fail to compile dependency '%a'." pp_ident m
  ) ;
    let (_,ctx) = Dko.unmarshal lc m in
      ( H.add envs m ctx ; ctx )

let get_deps () : string list =
  H.fold (
    fun md _ lst ->
      if ident_eq md !Global.name then lst
      else (string_of_ident md)::lst
  ) envs []

let export_and_clear () =
  ( if !Global.export then
      Dko.marshal (get_deps ()) (H.find envs !Global.name) ) ;
  H.clear envs

(******************************************************************************)

let get_infos lc m v =
  let env =
    try H.find envs m
    with Not_found -> import lc m
  in
    try ( H.find env v )
    with Not_found ->
      Global.fail lc "Cannot find symbol '%a.%a'." pp_ident m pp_ident v

let get_type lc m v =
  match get_infos lc m v with
    | Decl ty
    | Def (_,ty)
    | Decl_rw (ty,_,_,_) -> ty

(******************************************************************************)

let add lc v gst =
  let env = H.find envs !Global.name in
    if H.mem env v then
      if !Global.ignore_redecl then
        Global.debug 1 lc "Redeclaration ignored."
      else
        Global.fail lc "Already defined symbol '%a'." pp_ident v
    else
      H.add env v gst

let add_decl lc v ty    = add lc v (Decl ty)
let add_def lc v te ty  = add lc v (Def (te,ty))

let add_rw = function
  | [] -> ()
  | r::_ as rs ->
      let env = H.find envs !Global.name in
      let rwi = H.find env r.id in
        H.add env r.id (Matching.add_rules rwi rs)

(******************************************************************************)

let rules_iter f = (*FIXME*)
  H.iter (
    fun _ ht -> H.iter (
      fun _ infos ->
        match infos with
          | Decl_rw (_,rs,_,_) -> List.iter f rs
          | _ -> ()
    ) ht ) envs
