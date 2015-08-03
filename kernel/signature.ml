(** Global Environment *)

open Basics
open Term
open Rule

let ignore_redecl = ref false
let autodep = ref false

type signature_error =
  | FailToCompileModule of loc*ident
  | UnmarshalBadVersionNumber of loc*string
  | UnmarshalSysError of loc*string*string
  | UnmarshalUnknown of loc*string
  | SymbolNotFound of loc*ident*ident
  | AlreadyDefinedSymbol of loc*ident
  | CannotBuildDtree of Dtree.dtree_error
  | CannotAddRewriteRules of loc*ident
  | NonConfluentSystem of loc*rule2 list*string

exception SignatureError of signature_error

type dtree_or_def =
  | DoD_None
  | DoD_Def of term
  | DoD_Dtree of int*dtree

module H = Hashtbl.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

type rw_infos =
  | Constant of term
  | Definable of term * (rule_infos list*int*dtree) option

type t = { name:ident; tables:(rw_infos H.t) H.t }

let make name =
  let ht = H.create 19 in
    H.add ht name (H.create 251); { name=name; tables=ht; }

let get_name sg = sg.name

(******************************************************************************)

let marshal name deps env =
  try
    begin
      let out = open_out (string_of_ident name ^ ".dko" ) in
        Marshal.to_channel out Version.version [] ;
        Marshal.to_channel out deps [] ;
        Marshal.to_channel out env [Marshal.Closures] ;
        close_out out ;
        true
    end
  with
    | _ -> false

(* exception BadVersionNumber *)

let file_exists = Sys.file_exists

let rec find_dko_in_path name = function
  | [] -> failwith "find_dko"  (* Captured by the unmarshal function *)
  | dir :: path ->
      let filename = dir ^ "/" ^ name ^ ".dko" in
        if file_exists filename then
          open_in filename
        else
          find_dko_in_path name path

let find_dko name =
  (* First check in the current directory *)
  let filename = name ^ ".dko" in
    if file_exists filename then
      open_in filename
    else
      (* If not found in the current directory, search in load-path *)
      find_dko_in_path name (get_path())

let unmarshal (lc:loc) (m:string) : string list * rw_infos H.t =
  try
    begin
      let chan = find_dko m in
      let ver:string = Marshal.from_channel chan in
        if String.compare ver Version.version = 0 then
          begin
            let deps:string list = Marshal.from_channel chan in
            let ctx:rw_infos H.t = Marshal.from_channel chan in
              close_in chan ; (deps,ctx)
          end
        else raise (SignatureError (UnmarshalBadVersionNumber (lc,m)))
    end
  with
    | Sys_error s -> raise (SignatureError (UnmarshalSysError (lc,m,s)))
    | _ -> raise (SignatureError (UnmarshalUnknown (lc,m)))

let get_all_rules md =
  let (_,ht) = unmarshal dloc md in
  let aux _ rw rs = match rw with
    | Constant _
    | Definable (_,None) -> rs
    | Definable (_,Some(lst,_,_)) -> lst@rs
  in
    H.fold aux ht []
(******************************************************************************)

(* Recursively load a module and its dependencies*)
let rec import sg lc m =
  assert ( not (H.mem sg.tables m) ) ;
  debug "TODO: Confluence Check (import)"; (*FIXME*)
  (* If the [.dko] file is not found, try to compile it first.
   This hack is terrible. It uses system calls and can loop with circular dependencies. *)
  ( if !autodep && not ( Sys.file_exists ( string_of_ident m ^ ".dko" ) ) then
    if Sys.command ( "dkcheck -autodep -e " ^ string_of_ident m ^ ".dk" ) <> 0 then
        raise (SignatureError (FailToCompileModule (lc,m)))
    ) ;

    let (deps,ctx) = unmarshal lc (string_of_ident m) in
  H.add sg.tables m ctx;
  List.iter (
    fun dep -> if not (H.mem sg.tables m) then
      ignore (import sg lc (hstring dep))
      ) deps ;
  ctx

let get_deps sg : string list = (*only direct dependencies*)
  H.fold (
    fun md _ lst ->
      if ident_eq md sg.name then lst
      else (string_of_ident md)::lst
    ) sg.tables []

let export sg =
  marshal sg.name (get_deps sg) (H.find sg.tables sg.name)

(******************************************************************************)

let get_infos sg lc m v =
  let env =
    try H.find sg.tables m
    with Not_found -> import sg lc m
  in
    try ( H.find env v )
    with Not_found -> raise (SignatureError (SymbolNotFound (lc,m,v)))

let get_type sg lc m v =
  match get_infos sg lc m v with
    | Constant ty
    | Definable (ty,_) -> ty

let is_constant sg lc m v =
  match get_infos sg lc m v with
    | Constant _ -> true
    | Definable _ -> false

let get_dtree sg l m v =
  match get_infos sg l m v with
    | Constant _
    | Definable (_,None) -> None
    | Definable (_,Some(_,i,tr)) -> Some (i,tr)

(******************************************************************************)

let add sg lc v gst =
  Tpdb.add_constant sg.name v;
  let env = H.find sg.tables sg.name in
  if H.mem env v then
    ( if !ignore_redecl then debug "Redeclaration ignored."
      else raise (SignatureError (AlreadyDefinedSymbol (lc,v))) )
  else
    H.add env v gst

let add_declaration sg lc v ty = add sg lc v (Constant ty)
let add_definable sg lc v ty = add sg lc v (Definable (ty,None))

let add_rules sg lst : unit =
  let rs = map_error_list Dtree.to_rule_infos lst in
  match rs with
  | Err e -> raise (SignatureError (CannotBuildDtree e))
  | OK [] -> ()
  | OK (r::_ as rs) ->
    let env = H.find sg.tables sg.name in
    let infos = try H.find env r.id with Not_found ->
      raise (SignatureError (SymbolNotFound(r.l,sg.name,r.id)))
    in
    let (ty,rules) = match infos with
      | Definable (ty,None) -> ( ty , rs )
      | Definable (ty,Some(mx,_,_)) -> ( ty , mx@rs )
      | Constant _ ->
        raise (SignatureError (CannotAddRewriteRules (r.l,r.id)))
    in
    match Dtree.of_rules rules with
    | OK (n,tree) ->
      begin
        Tpdb.add_rules rs;
        match Tpdb.check () with
        | OK () -> H.add env r.id (Definable (ty,Some(rules,n,tree)))
        | Err file -> raise (SignatureError (NonConfluentSystem (r.l,lst,file)))
      end
    | Err e -> raise (SignatureError (CannotBuildDtree e))
