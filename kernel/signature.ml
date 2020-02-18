(** Global Environment *)

open Basic
open Term
open Rule

let d_module = Debug.register_flag "Module"

let fail_on_symbol_not_found = ref true

type signature_error =
  | UnmarshalBadVersionNumber of loc * string
  | UnmarshalSysError         of loc * string * string
  | UnmarshalUnknown          of loc * string
  | SymbolNotFound            of loc * name
  | AlreadyDefinedSymbol      of loc * name
  | CannotMakeRuleInfos       of Rule.rule_error
  | CannotBuildDtree          of Dtree.dtree_error
  | CannotAddRewriteRules     of loc * name
  | ConfluenceErrorImport     of loc * mident * Confluence.confluence_error
  | ConfluenceErrorRules      of loc * rule_infos list * Confluence.confluence_error
  | GuardNotSatisfied         of loc * term * term
  | CouldNotExportModule      of mident * string
  | PrivateSymbol             of loc * name

exception Signature_error of signature_error

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

type staticity = Static | Definable | Injective
type scope     = Public | Private
type locality  = Global | Local

(** The pretty printer for the type [staticity] *)
let pp_staticity fmt s =
  Format.fprintf fmt "%s" (if s=Static then "Static" else "Definable")

type rw_infos =
  {
    stat          : staticity;
    ty            : term;
    scope         : scope;
    locality      : locality;
    rules         : rule_infos list;
    decision_tree : Dtree.t option
  }

type t =
  {
    name   : mident;
    file   : string;
    (** [tables] maps module ident to the hastable of their symbols.
        It should only contain a single entry for each module.
        Each module's hashtable should only contain a single entry
        for each of its symbols. *)
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
    then raise (Signature_error (UnmarshalBadVersionNumber (lc,m)));
    let deps:string list         = Marshal.from_channel chan in
    let ctx:rw_infos HId.t       = Marshal.from_channel chan in
    let ext:rule_infos list list = Marshal.from_channel chan in
    close_in chan; (deps,ctx,ext)
  with
  | Sys_error s -> raise (Signature_error (UnmarshalSysError (lc,m,s)))
  | Signature_error s -> raise (Signature_error s)
  | _ -> raise (Signature_error (UnmarshalUnknown (lc,m)))


let fold_symbols f sg =
  HMd.fold (fun md table t -> HId.fold (f md) table t) sg.tables

let iter_symbols f sg =
  fold_symbols (fun md id rw () -> f md id rw) sg ()



(******************************************************************************)

let check_confluence_on_import lc (md:mident) (ctx:rw_infos HId.t) : unit =
  let aux id infos =
    let cst = mk_name md id in
    Confluence.add_constant cst;
    Confluence.add_rules infos.rules
  in
  HId.iter aux ctx;
  Debug.debug Confluence.d_confluence
    "Checking confluence after loading module '%a'..." pp_mident md;
  try Confluence.check () with
  | Confluence.Confluence_error e -> raise (Signature_error (ConfluenceErrorImport (lc,md,e)))

let add_external_declaration sg lc cst scope locality stat ty =
  try
    let env = HMd.find sg.tables (md cst) in
    if HId.mem env (id cst)
    then raise (Signature_error (AlreadyDefinedSymbol (lc, cst)))
    else HId.replace env (id cst) {stat; ty; scope; locality; rules=[]; decision_tree=None}
  with Not_found ->
    HMd.replace sg.tables (md cst) (HId.create 11);
    let env = HMd.find sg.tables (md cst) in
    HId.replace env (id cst) {stat; ty; scope; locality; rules=[]; decision_tree=None}

(* Recursively load a module and its dependencies*)
let rec import sg lc m =
  if HMd.mem sg.tables m
  then Debug.(debug d_warn "Trying to import the already loaded module %s." (string_of_mident m))
  else
    let (deps,ctx,ext) = unmarshal lc (string_of_mident m) in
    HMd.replace sg.tables m ctx;
    List.iter ( fun dep0 ->
        let dep = mk_mident dep0 in
        if not (HMd.mem sg.tables dep) then import sg lc dep
      ) deps ;
    Debug.(debug d_module "Loading module '%a'..." pp_mident m);
    List.iter (fun rs -> add_rule_infos sg rs) ext;
    check_confluence_on_import lc m ctx

and add_rule_infos sg (lst:rule_infos list) : unit =
  match lst with
  | [] -> ()
  | (r::_ as rs) ->
    let infos, env =
      try get_info_env sg r.l r.cst
      with
      | Signature_error (SymbolNotFound _)
      | Signature_error (UnmarshalUnknown _) when not !fail_on_symbol_not_found ->
         add_external_declaration sg r.l r.cst Public Global Definable (mk_Kind);
         get_info_env sg r.l r.cst
    in
    if infos.stat = Static && !fail_on_symbol_not_found
    then raise (Signature_error (CannotAddRewriteRules (r.l,r.cst)));
    HId.replace env (id r.cst) {infos with rules = infos.rules @ rs; decision_tree= None}

and compute_dtree sg (lc:Basic.loc) (cst:Basic.name) : Dtree.t option =
  let infos, env = get_info_env sg lc cst in
  match infos.decision_tree, infos.rules with
  (* Non-empty set of rule but decision trees not computed *)
  | None, (_::_ as rules) ->
    let trees =
      try Dtree.of_rules rules
      with Dtree.Dtree_error e -> raise (Signature_error (CannotBuildDtree e))
    in
    HId.replace env (id cst) {infos with decision_tree=Some trees};
    Some trees
| t, _ -> t

and get_info_env sg lc cst =
  let md = md cst in
  let env =  (* Fetch module, import it if it's missing *)
    try HMd.find sg.tables md
    with Not_found -> import sg lc md; HMd.find sg.tables md
  in
  try (HId.find env (id cst), env)
  with Not_found -> raise (Signature_error (SymbolNotFound (lc,cst)))

and get_infos sg lc cst = fst (get_info_env sg lc cst)

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


let import_signature sg sg_ext =
  HMd.iter (fun m hid ->
      if not (HMd.mem sg.tables m) then
        HMd.replace sg.tables m (HId.copy hid)) sg_ext.tables;
  List.iter (fun rs -> add_rule_infos sg rs) sg_ext.external_rules

let export sg =
  let mod_table = HMd.find sg.tables sg.name in
  HId.iter
    (fun id t -> if t.locality = Local then HId.remove mod_table id)
    mod_table;
  if not (marshal sg.file (get_deps sg) mod_table sg.external_rules)
  then raise (Signature_error (CouldNotExportModule (sg.name, sg.file)))

(******************************************************************************)

let is_injective sg lc cst =
  match (get_infos sg lc cst).stat with
  | Static | Injective -> true
  | Definable          -> false

let get_type sg lc cst =
  let infos = get_infos sg lc cst in
  if infos.scope = Public || md cst = sg.name
  then infos.ty
  else raise (Signature_error (PrivateSymbol(lc,cst)))

let get_rules sg lc cst = (get_infos sg lc cst).rules

let get_dtree sg lc cst =
  try
    match compute_dtree sg lc cst with
    | None -> Dtree.empty
    | Some trees -> trees
  with e ->
    if not !fail_on_symbol_not_found then Dtree.empty else raise e

(******************************************************************************)

let add_declaration sg lc v scope locality stat ty =
  let cst = mk_name sg.name v in
  add_external_declaration sg lc cst scope locality stat ty

let add_rules sg = function
  | [] -> ()
  | r :: _ as rs ->
    try
      add_rule_infos sg rs;
      if not (mident_eq sg.name (md r.cst)) then
        sg.external_rules <- rs::sg.external_rules;
      Confluence.add_rules rs;
      Debug.(debug Confluence.d_confluence
               "Checking confluence after adding rewrite rules on symbol '%a'"
               pp_name r.cst);
      try Confluence.check ()
      with Confluence.Confluence_error e -> raise (Signature_error (ConfluenceErrorRules (r.l,rs,e)))
    with Rule_error e -> raise (Signature_error (CannotMakeRuleInfos e))
