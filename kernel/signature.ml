(** Global Environment *)

open Basic
open Term
open Rule
open Dtree

let ignore_redecl = ref false
let autodep = ref false

type signature_error =
  | FailToCompileModule of loc * mident
  | UnmarshalBadVersionNumber of loc * string
  | UnmarshalSysError of loc * string * string
  | UnmarshalUnknown of loc * string
  | SymbolNotFound of loc * name
  | AlreadyDefinedSymbol of loc * ident
  | CannotMakeRuleInfos of Rule.rule_error
  | CannotBuildDtree of Dtree.dtree_error
  | CannotAddRewriteRules of loc * ident
  | ConfluenceErrorImport of loc * mident * Confluence.confluence_error
  | ConfluenceErrorRules of loc * rule_infos list * Confluence.confluence_error

exception SignatureError of signature_error

type dtree_or_def =
  | DoD_None
  | DoD_Def of term
  | DoD_Dtree of int*dtree

module HMd = Hashtbl.Make(
struct
  type t        = mident
  let equal     = mident_eq
  let hash      = Hashtbl.hash
end )

module HId = Hashtbl.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

type staticity = Static | Definable

type rw_infos =
  {
    stat: staticity;
    ty: term;
    rule_opt_info: (rule_infos list*int*dtree) option
  }

type t = { name:mident;
           tables:(rw_infos HId.t) HMd.t;
           mutable external_rules:rule_infos list list; }

let make name =
  let ht = HMd.create 19 in
  HMd.add ht name (HId.create 251); { name=name; tables=ht; external_rules=[]; }

let get_name sg = sg.name

let get_external_rules sg = sg.external_rules

let get_tables sg=
  let res=ref [] in
  HMd.iter (fun a tb ->
    HId.iter (fun b x ->
      if not (List.exists (fun (n,_,_,_) -> name_eq n (mk_name a b)) !res)
      then res:=(mk_name a b,x.stat,x.ty,x.rule_opt_info)::(!res)
    ) tb
  ) sg.tables;
  !res
  
(******************************************************************************)

let add_rule_infos sg (lst:rule_infos list) : unit =
  match lst with
  | [] -> ()
  | (r::_ as rs) ->
    let env =
      try HMd.find sg.tables (md r.cst)
      with Not_found -> assert false in (*should not happen if the dependencies are loaded before*)
    let infos = try ( HId.find env (id r.cst) )
      with Not_found -> assert false in
    let ty = infos.ty in
    if (infos.stat = Static) then
      raise (SignatureError (CannotAddRewriteRules (r.l,(id r.cst))));
    let rules = match infos.rule_opt_info with
      | None -> rs
      | Some(mx,_,_) -> mx@rs
    in
    match Dtree.of_rules rules with
    | OK (n,tree) ->
       HId.add env (id r.cst)
         {stat = infos.stat; ty=ty; rule_opt_info = Some(rules,n,tree)}
    | Err e -> raise (SignatureError (CannotBuildDtree e))

(******************************************************************************)

let marshal (name:mident) (deps:string list) (env:rw_infos HId.t) (ext:rule_infos list list) : bool =
  try
    begin
      let out = open_out (string_of_mident name ^ ".dko" ) in
        Marshal.to_channel out Version.version [] ;
        Marshal.to_channel out deps [] ;
        Marshal.to_channel out env [] ;
        Marshal.to_channel out ext [] ;
        close_out out ;
        true
    end
  with
    | _ -> false

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

let unmarshal (lc:loc) (m:string) : string list * rw_infos HId.t * rule_infos list list =
  try
    begin
      let chan = find_dko m in
      let ver:string = Marshal.from_channel chan in
        if String.compare ver Version.version = 0 then
          begin
            let deps:string list = Marshal.from_channel chan in
            let ctx:rw_infos HId.t = Marshal.from_channel chan in
            let ext:rule_infos list list= Marshal.from_channel chan in
              close_in chan ; (deps,ctx,ext)
          end
        else raise (SignatureError (UnmarshalBadVersionNumber (lc,m)))
    end
  with
    | Sys_error s -> raise (SignatureError (UnmarshalSysError (lc,m,s)))
    | SignatureError s -> raise (SignatureError s)
    | _ -> raise (SignatureError (UnmarshalUnknown (lc,m)))

(******************************************************************************)

let check_confluence_on_import lc (md:mident) (ctx:rw_infos HId.t) : unit =
  let aux id infos =
    let cst = mk_name md id in
    Confluence.add_constant cst;
    match infos.rule_opt_info with
    | None -> ()
    | Some (rs,_,_) -> Confluence.add_rules rs
  in
  HId.iter aux ctx;
  debug 1 "Checking confluence after loading module '%a'..." pp_mident md;
  match Confluence.check () with
  | OK () -> ()
  | Err err -> raise (SignatureError (ConfluenceErrorImport (lc,md,err)))

(* Recursively load a module and its dependencies*)
let rec import sg lc m =
  assert ( not (HMd.mem sg.tables m) ) ;

  (* If the [.dko] file is not found, try to compile it first.
     This hack is terrible. It uses system calls and can loop with circular dependencies.
     Also, this hack supposes that the module name and the file name are the same.*)
  ( if !autodep && not ( Sys.file_exists ( string_of_mident m ^ ".dko" ) ) then
      if Sys.command ( "dkcheck -autodep -e " ^ string_of_mident m ^ ".dk" ) <> 0 then
        raise (SignatureError (FailToCompileModule (lc,m)))
  ) ;

  let (deps,ctx,ext) = unmarshal lc (string_of_mident m) in
  HMd.add sg.tables m ctx;
  List.iter ( fun dep0 ->
      let dep = mk_mident dep0 in
      if not (HMd.mem sg.tables dep) then import sg lc dep
    ) deps ;
  debug 1 "Loading module '%a'..." pp_mident m;
  List.iter (fun rs -> add_rule_infos sg rs) ext;
  check_confluence_on_import lc m ctx

let get_deps sg : string list = (*only direct dependencies*)
  HMd.fold (
    fun md _ lst ->
      if mident_eq md sg.name then lst
      else (string_of_mident md)::lst
    ) sg.tables []

let export sg =
  marshal sg.name (get_deps sg) (HMd.find sg.tables sg.name) sg.external_rules

(******************************************************************************)

let get_infos sg lc cst =
  let md = md cst in
  let env =
    try HMd.find sg.tables md
    with Not_found -> import sg lc md; HMd.find sg.tables md
  in
    try ( HId.find env (id cst))
    with Not_found -> raise (SignatureError (SymbolNotFound (lc,cst)))

let is_injective sg lc cst =
  match (get_infos sg lc cst).stat with
  | Static -> true
  | Definable -> false

let get_type sg lc cst = (get_infos sg lc cst).ty

let pred_true: Rule.rule_name -> bool = fun x -> true

let get_dtree sg ?select:(pred=pred_true) l cst =
  match (get_infos sg l cst).rule_opt_info with
  | None -> None
  | Some(rules,i,tr) ->
    if pred == pred_true then
      Some (i,tr)
    else
      let rules' = List.filter (fun (r:Rule.rule_infos) -> pred r.name) rules in
      if List.length rules' == List.length rules
      then Some (i,tr)
      else
        (* A call to Dtree.of_rules must be made with a non-empty list *)
        match rules' with
        | [] -> None
        | _ -> match Dtree.of_rules rules' with
               | OK (n,tree) -> Some(n,tree)
               | Err e -> raise (SignatureError (CannotBuildDtree e))


(******************************************************************************)

let add_declaration sg lc v st ty =
  let cst = mk_name sg.name v in
  Confluence.add_constant cst;
  let env = HMd.find sg.tables sg.name in
  if HId.mem env v then
    ( if !ignore_redecl then debug 1 "Redeclaration ignored."
      else raise (SignatureError (AlreadyDefinedSymbol (lc,v))) )
  else
    HId.add env v {stat=st; ty=ty; rule_opt_info=None}

let add_rules sg lst : unit =
  let rs = map_error_list Rule.to_rule_infos lst in
  match rs with
  | Err e -> raise (SignatureError (CannotMakeRuleInfos e))
  | OK [] -> ()
  | OK (r::_ as rs) ->
    begin
      add_rule_infos sg rs;
      if not (mident_eq sg.name (md r.cst)) then
        sg.external_rules <- rs::sg.external_rules;
      Confluence.add_rules rs;
      debug 1 "Checking confluence after adding rewrite rules on symbol '%a'"
        pp_name r.cst;
      match Confluence.check () with
      | OK () -> ()
      | Err err -> raise (SignatureError (ConfluenceErrorRules (r.l,rs,err)))
    end
