open Types
open Printf

module H = Hashtbl.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

(* *** Environment management *** *)

let envs : (rw_infos H.t) H.t = H.create 19
let init name = H.add envs name (H.create 251)

(* *** Modules *** *)

let import lc m =
  assert ( not (H.mem envs m) );
  (* If the [.dko] file is not found, try to compile it first.
   This hack is terrible. It uses system calls and can loop with circular dependencies. *)
  begin
    if not ( Sys.file_exists ( string_of_ident m ^ ".dko" ) ) && !Global.autodep then
      ignore ( Sys.command ( "dkcheck -e " ^ string_of_ident m ^ ".dk" ) )
  end ;
  try
    (* If the [.dko] file is not found, try to compile it first.
     This hack is terrible. It uses system calls and can loop with circular dependencies. *)
    begin
      if not ( Sys.file_exists ( string_of_ident m ^ ".dko" ) ) && !Global.autodep then
        ignore ( Sys.command ( "dkcheck -e " ^ string_of_ident m ^ ".dk" ) )
          end ;
      let chan = open_in ( string_of_ident m ^ ".dko" ) in
      let ctx:rw_infos H.t = Marshal.from_channel chan in
        close_in chan ;
        H.add envs m ctx ;
        ctx
  with _ ->
    Global.fail lc "Fail to open module '%a'." pp_ident m

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

(* *** Add *** *)

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

(* ***** *)

let rules_iter f =
  H.iter (
    fun _ ht -> H.iter (
      fun _ infos ->
        match infos with
          | Decl_rw (_,rs,_,_) -> List.iter f rs
          | _ -> ()
    ) ht ) envs
