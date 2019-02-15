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
  | AlreadyDefinedSymbol  of loc * ident
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

type rw_infos =
  {
    stat          : staticity;
    ty            : term;
    rule_opt_info : (rule_infos list* Dtree.t) option
  }

type symbol_infos =
  {
    ident : name;
    stat  : staticity;
    ty    : term;
    rules : rule_infos list
  }

type t = { name   : mident;
           file   : string;
           tables : (rw_infos HId.t) HMd.t;
           mutable external_rules:rule_infos list list; }

let make file =
  let name = mk_mident file in
  let tables = HMd.create 19 in
  HMd.add tables name (HId.create 251);
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

let get_rule_infos infos =
  match infos.rule_opt_info with None -> [] | Some (rs,_) -> rs

let access_signature sg =
  let default_first a =
    function
    | None      -> a
    | Some(x,_) -> x
  in
  let add_in_symbol_infos (f : name) (r : rule_infos) =
    let rec aux (acc : symbol_infos list) =
      function
      | []    -> assert false
      | s::tl ->
         if s.ident = f
         then {s with rules = r::s.rules}::(acc@tl)
         else aux (s::acc) tl
    in aux []
  in
  let symbol_infos_crafting (md : mident) (id : ident) (r : rw_infos) =
    { ident  = mk_name md id
    ; ty    = r.ty
    ; rules = default_first [] r.rule_opt_info
    ; stat  = r.stat}
  in
  let res = ref [] in
  HMd.iter
    (fun md t ->
      HId.iter
        (fun id r ->
          res:=(symbol_infos_crafting md id r)::!res
        ) t
    ) sg.tables;
  List.iter
    (fun l ->
      List.iter
        (fun r -> res := add_in_symbol_infos r.cst r !res
        ) l
    ) sg.external_rules;
  !res



(******************************************************************************)

let check_confluence_on_import lc (md:mident) (ctx:rw_infos HId.t) : unit =
  let aux id infos =
    let cst = mk_name md id in
    Confluence.add_constant cst;
    match infos.rule_opt_info with
    | None -> ()
    | Some (rs,_) -> Confluence.add_rules rs
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
    HMd.add sg.tables m ctx;
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
    let ty = infos.ty in
    if infos.stat = Static
    then raise (SignatureError (CannotAddRewriteRules (r.l,(id r.cst))));
    let rules = match infos.rule_opt_info with
      | None -> rs
      | Some(mx,_) -> mx@rs
    in
    let trees =
      try Dtree.of_rules rules
      with Dtree.DtreeError e -> raise (SignatureError (CannotBuildDtree e))
    in
    HId.add env (id r.cst) {stat = infos.stat; ty=ty; rule_opt_info = Some(rules,trees)}

and get_env sg lc cst =
  let md = md cst in
  try HMd.find sg.tables md
  with Not_found -> import sg lc md; HMd.find sg.tables md

and get_info_env lc env cst =
  try HId.find env (id cst)
  with Not_found -> raise (SignatureError (SymbolNotFound (lc,cst)))

let get_infos sg lc cst = get_info_env lc (get_env sg lc cst) cst

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
  if not (marshal sg.file (get_deps sg) (HMd.find sg.tables sg.name) sg.external_rules)
  then raise (SignatureError (CouldNotExportModule sg.file))

(******************************************************************************)

let is_static sg lc cst =
  match (get_infos sg lc cst).stat with
  | Static    -> true
  | Definable -> false

let get_type sg lc cst = (get_infos sg lc cst).ty

let get_dtree sg rule_filter l cst =
  match (get_infos sg l cst).rule_opt_info, rule_filter with
  | None             , _      -> Dtree.empty
  | Some(_,trees)    , None   -> trees
  | Some(rules,trees), Some f ->
    let rules' = List.filter (fun (r:Rule.rule_infos) -> f r.name) rules in
    if List.length rules' == List.length rules then trees
    else
      try Dtree.of_rules rules'
      with Dtree.DtreeError e -> raise (SignatureError (CannotBuildDtree e))

let get_rules sg lc cst =
  match (get_infos sg lc cst).rule_opt_info with
  | None -> []
  | Some (rs,_) -> rs

(******************************************************************************)

let add_declaration sg lc v st ty =
  let cst = mk_name sg.name v in
  Confluence.add_constant cst;
  let env = HMd.find sg.tables sg.name in
  if HId.mem env v
  then raise (SignatureError (AlreadyDefinedSymbol (lc,v)))
  else HId.add env v {stat=st; ty=ty; rule_opt_info=None}

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
