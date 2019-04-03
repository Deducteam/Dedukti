(** Global Environment *)

open Basic
open Term
open Rule

type Debug.flag += D_module
let _ = Debug.register_flag D_module "Module"

type signature_error =
  | UnmarshalBadVersionNumber of loc * string
  | UnmarshalSysError     of loc * string * string
  | UnmarshalUnknown      of loc * string
  | SymbolNotFound        of loc * name
  | AlreadyDefinedSymbol  of loc * name
  | CannotMakeRuleInfos   of Rule.rule_error
  | CannotBuildDtree      of Dtree.dtree_error
  | CannotAddRewriteRules of loc * ident
  | ConfluenceErrorImport of loc * mident * Confluence.confluence_error
  | ConfluenceErrorRules  of loc * rule_infos list * Confluence.confluence_error
  | GuardNotSatisfied     of loc * term * term
  | CouldNotExportModule  of string

exception SignatureError of signature_error

module HMd = Hashtbl.Make(
  struct
    type t    = mident
    let equal = mident_eq
    let hash  = Hashtbl.hash
  end )

module HId = Hashtbl.Make(
  struct
    type t    = ident
    let equal = ident_eq
    let hash  = Hashtbl.hash
  end )

type staticity = Static | Definable

(** The pretty printer for the type [staticity] *)
let pp_staticity fmt s =
  Format.fprintf fmt "%s" (if s=Static then "Static" else "Definable")

type symbol_infos =
  {
    name  : name;
    stat  : staticity;
    ty    : term;
    rules : rule_infos list;
  }

type rw_infos =
  {
    stat          : staticity;
    ty            : term;
    rules         : rule_infos list;
    decision_tree : Dtree.t option * bool
  }

type t =
  {
    name   : mident;
    file   : string;
    tables : (rw_infos HId.t) HMd.t;
    mutable external_rules:rule_infos list list;
  }

let make file =
  let name = mk_mident file in
  let tables = HMd.create 19 in
  HMd.replace tables name (HId.create 251);
  { name; file; tables; external_rules=[]; }

let get_name sg = sg.name

(******************************************************************************)

let marshal (file:string) (deps:string list) (env:rw_infos HId.t) (ext:rule_infos list list) : bool =
  try
    let file = (try Filename.chop_extension file with _ -> file) ^ ".dko" in
    let oc = open_out file in
    Marshal.to_channel oc Version.version [];
    Marshal.to_channel oc deps [];
    Marshal.to_channel oc env [];
    Marshal.to_channel oc ext [];
    close_out oc; true
  with _ -> false

let file_exists = Sys.file_exists

let rec find_dko_in_path name = function
  | [] -> failwith "find_dko"  (* Captured by the unmarshal function *)
  | dir :: path ->
      let filename = dir ^ "/" ^ name ^ ".dko" in
      if file_exists filename
      then open_in filename
      else find_dko_in_path name path

let find_dko name =
  let filename = name ^ ".dko" in
  if file_exists filename (* First check in the current directory *)
  then open_in filename
  else find_dko_in_path name (get_path())
  (* If not found in the current directory, search in load-path *)

let unmarshal (lc:loc) (m:string) : string list * rw_infos HId.t * rule_infos list list =
  try
    let chan = find_dko m in
    let ver:string = Marshal.from_channel chan in
    if String.compare ver Version.version <> 0
    then raise (SignatureError (UnmarshalBadVersionNumber (lc,m)));
    let deps:string list         = Marshal.from_channel chan in
    let ctx:rw_infos HId.t       = Marshal.from_channel chan in
    let ext:rule_infos list list = Marshal.from_channel chan in
    close_in chan; (deps,ctx,ext)
  with
  | Sys_error s -> raise (SignatureError (UnmarshalSysError (lc,m,s)))
  | SignatureError s -> raise (SignatureError s)
  | _ -> raise (SignatureError (UnmarshalUnknown (lc,m)))

let symbols_of sg =
  HMd.fold (fun md ->
      HId.fold (fun id (r:rw_infos) ->
          List.cons
            { name  = mk_name md id;
              stat  = r.stat;
              ty    = r.ty;
              rules =r.rules}))
    sg.tables []



(******************************************************************************)

let check_confluence_on_import lc (md:mident) (ctx:rw_infos HId.t) : unit =
  let aux id infos =
    let cst = mk_name md id in
    Confluence.add_constant cst;
    Confluence.add_rules infos.rules
  in
  HId.iter aux ctx;
  Debug.debug Confluence.D_confluence
    "Checking confluence after loading module '%a'..." pp_mident md;
  try Confluence.check () with
  | Confluence.ConfluenceError e -> raise (SignatureError (ConfluenceErrorImport (lc,md,e)))

(* Recursively load a module and its dependencies*)
let rec import sg lc m =
  if HMd.mem sg.tables m
  then Debug.(debug D_warn "Trying to import the already loaded module %s." (string_of_mident m))
  else
    let (deps,ctx,ext) = unmarshal lc (string_of_mident m) in
    HMd.replace sg.tables m ctx;
    List.iter ( fun dep0 ->
        let dep = mk_mident dep0 in
        if not (HMd.mem sg.tables dep) then import sg lc dep
      ) deps ;
    Debug.(debug D_module "Loading module '%a'..." pp_mident m);
    List.iter (fun rs -> add_rule_infos sg rs) ext;
    check_confluence_on_import lc m ctx

and add_rule_infos sg (lst:rule_infos list) : unit =
  match lst with
  | [] -> ()
  | (r::_ as rs) ->
    let env = get_env sg r.l r.cst in
    let infos : rw_infos = get_info_env r.l env r.cst in
    if infos.stat = Static
    then raise (SignatureError (CannotAddRewriteRules (r.l,(id r.cst))));
    HId.replace env (id r.cst)
      {infos with rules = infos.rules @ rs; decision_tree=(fst infos.decision_tree, false)}

and compute_dtree sg (lc:Basic.loc) (cst:Basic.name) : Dtree.t option =
  let env = get_env sg lc cst in
  let infos = get_info_env lc env cst in
  match infos.decision_tree with
  | t, true  -> t
  | _, false ->
    let trees =
      try Dtree.of_rules infos.rules
      with Dtree.DtreeError e -> raise (SignatureError (CannotBuildDtree e))
    in
    HId.replace env (id cst)
      {infos with decision_tree=(Some trees, true)};
    Some trees

and get_env sg lc cst =
  let md = md cst in
  try HMd.find sg.tables md
  with Not_found -> import sg lc md; HMd.find sg.tables md

and get_info_env lc env cst =
  try HId.find env (id cst)
  with Not_found -> raise (SignatureError (SymbolNotFound (lc,cst)))

and get_infos sg lc cst = get_info_env lc (get_env sg lc cst) cst

(******************************************************************************)

let get_md_deps (lc:loc) (md:mident) =
  let (deps,_,_) = unmarshal lc (string_of_mident md) in
  List.map mk_mident deps

let get_deps sg : string list = (*only direct dependencies*)
  HMd.fold (
    fun md _ lst ->
      if mident_eq md sg.name then lst
      else (string_of_mident md)::lst
    ) sg.tables []

let export sg =
  let mod_table = HMd.find sg.tables sg.name in
  (* Making sure all decision trees are computed before exporting. *)
  HId.iter
    (fun id t ->
       if not (snd t.decision_tree)
       then ignore(compute_dtree sg dloc (mk_name sg.name id)))
    mod_table;
  if not (marshal sg.file (get_deps sg) mod_table sg.external_rules)
  then raise (SignatureError (CouldNotExportModule sg.file))

(******************************************************************************)

let is_static sg lc cst =
  match (get_infos sg lc cst).stat with
  | Static    -> true
  | Definable -> false

let get_type sg lc cst = (get_infos sg lc cst).ty

let get_dtree sg rule_filter l cst =
  match compute_dtree sg l cst, rule_filter with
  | None       , _      -> Dtree.empty
  | Some(trees), None   -> trees
  | Some(trees), Some f ->
     let rules = (get_infos sg l cst).rules in
     let rules' = List.filter (fun (r:Rule.rule_infos) -> f r.name) rules in
     if List.length rules' == List.length rules then trees
     else
       try Dtree.of_rules rules'
       with Dtree.DtreeError e -> raise (SignatureError (CannotBuildDtree e))

let get_rules sg lc cst =
  (get_infos sg lc cst).rules

(******************************************************************************)

let add_external_declaration sg lc cst stat ty =
  try
    Confluence.add_constant cst;
    let env = HMd.find sg.tables (md cst) in
    if HId.mem env (id cst)
    then raise (SignatureError (AlreadyDefinedSymbol (lc, cst)))
    else HId.replace env (id cst) {stat; ty; rules=[]; decision_tree=(None,true)}
  with Not_found ->
    HMd.replace sg.tables (md cst) (HId.create 11);
    let env = HMd.find sg.tables (md cst) in
    HId.replace env (id cst) {stat; ty; rules=[]; decision_tree=(None,true)}

let add_declaration sg lc v st ty =
  let cst = mk_name sg.name v in
  add_external_declaration sg lc cst st ty

let add_rules sg = function
  | [] -> ()
  | r :: _ as rs ->
    try
      add_rule_infos sg rs;
      if not (mident_eq sg.name (md r.cst)) then
        sg.external_rules <- rs::sg.external_rules;
      let open Confluence in
      add_rules rs;
      Debug.(debug D_confluence
               "Checking confluence after adding rewrite rules on symbol '%a'"
               pp_name r.cst);
      try check ()
      with ConfluenceError e -> raise (SignatureError (ConfluenceErrorRules (r.l,rs,e)))
    with RuleError e -> raise (SignatureError (CannotMakeRuleInfos e))
