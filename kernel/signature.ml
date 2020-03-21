(** Global Environment *)

open Basic
open Term
open Rule

let d_module = Debug.register_flag "Module"

type file = string

let fail_on_symbol_not_found = ref true

type signature_error =
  | UnmarshalBadVersionNumber of loc * string
  | UnmarshalSysError     of loc * string * string
  | UnmarshalUnknown      of loc * string
  | SymbolNotFound        of loc * name
  | AlreadyDefinedSymbol  of loc * name
  | CannotMakeRuleInfos   of Rule.rule_error
  | CannotBuildDtree      of Dtree.dtree_error
  | CannotAddRewriteRules of loc * name
  | ConfluenceErrorImport of loc * mident * Confluence.confluence_error
  | ConfluenceErrorRules  of loc * rule_infos list * Confluence.confluence_error
  | GuardNotSatisfied     of loc * term * term
  | CannotExportModule    of mident * exn
  | PrivateSymbol         of loc * name
  | ExpectedACUSymbol     of loc * name

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

type staticity = Static | Definable of algebra | Injective

let algebra_of_staticity = function
  | Static
  | Injective   -> Free
  | Definable a -> a

type scope     = Public | Private

(** The pretty printer for the type [staticity] *)
(* let pp_staticity fmt s =
 *   Format.fprintf fmt "%s" (if s=Static then "Static" else "Definable") *)

type rw_infos =
  {
    stat          : staticity;
    ty            : term;
    scope         : scope;
    rules         : rule_infos list;
    decision_tree : Dtree.t option
  }

type t =
  {
    md     : mident;
    (** [tables] maps module ident to the hastable of their symbols.
        It should only contain a single entry for each module.
        Each module's hashtable should only contain a single entry
        for each of its symbols. *)
    tables : (rw_infos HId.t) HMd.t;

    mutable external_rules:rule_infos list list;

    get_file : loc -> mident -> string
  }

let make md get_file =
  let tables = HMd.create 19 in
  HMd.replace tables md (HId.create 251);
  { md; tables; external_rules=[]; get_file}

let get_name sg = sg.md

(******************************************************************************)

let marshal : mident -> mident list -> rw_infos HId.t -> rule_infos list list -> out_channel -> unit =
  fun md deps env ext oc ->
  try
    Marshal.to_channel oc Version.version [];
    Marshal.to_channel oc deps [];
    Marshal.to_channel oc env [];
    Marshal.to_channel oc ext []
  with e -> raise @@ Signature_error (CannotExportModule(md,e))

let unmarshal (lc:loc) (file:string) : mident list * rw_infos HId.t * rule_infos list list =
  try
    let ic = open_in file in
    let ver:string = Marshal.from_channel ic in
    if String.compare ver Version.version <> 0
    then raise (Signature_error (UnmarshalBadVersionNumber (lc,file)));
    let deps:mident list         = Marshal.from_channel ic in
    let ctx:rw_infos HId.t       = Marshal.from_channel ic in
    let ext:rule_infos list list = Marshal.from_channel ic in
    close_in ic; (deps,ctx,ext)
  with
  | Sys_error s -> raise (Signature_error (UnmarshalSysError (lc,file,s)))
  | Signature_error s -> raise (Signature_error s)


let fold_symbols f sg =
  HMd.fold (fun md table t -> HId.fold (f md) table t) sg.tables

let iter_symbols f sg =
  fold_symbols (fun md id rw () -> f md id rw) sg ()

(******************************************************************************)

(* let get_type_from_AC (ty:term) =
 *   match ty with
 *   | Pi(_,_,t,_) -> t
 *   | _ -> assert false *)

let to_rule_infos_aux (r:unit rule) =
  try Rule.to_rule_infos r
  with Rule_error e -> raise (Signature_error (CannotMakeRuleInfos e))

let comm_rule (name:name) =
  to_rule_infos_aux
    { name=Gamma(true,mk_name (md name) (mk_ident ("comm_" ^ (string_of_ident (id name)))));
      ctx=[(dloc,mk_ident "x",()); (dloc,mk_ident "y",())];
      pat=Pattern (dloc, name,
                   [ Var (dloc,mk_ident "x",0,[]);
                     Var (dloc,mk_ident "y",1,[]) ]);
      rhs=mk_App (mk_Const dloc name)
                 (mk_DB dloc (mk_ident "y") 1)
                 [mk_DB dloc (mk_ident "x") 0]
    }

let asso_rule (name:name) =
  to_rule_infos_aux
    { name=Gamma(true,mk_name (md name) (mk_ident ("asso_" ^ (string_of_ident (id name)))));
      ctx=[ (dloc, (mk_ident "x"),());
            (dloc, (mk_ident "y"),());
            (dloc, (mk_ident "z"),()) ];
      pat=Pattern (dloc, name,
                   [ Pattern (dloc, name,
                              [ Var (dloc,mk_ident "x",0,[]);
                                Var (dloc,mk_ident "y",1,[]) ] );
                     Var (dloc,(mk_ident "z"),2,[]) ] );
      rhs=mk_App (mk_Const dloc name)
                 (mk_DB dloc (mk_ident "x") 0)
                 [mk_App (mk_Const dloc name)
                         (mk_DB dloc (mk_ident "y") 1)
                         [(mk_DB dloc (mk_ident "z") 2)] ]
    }

let neu1_rule (name:name) (_:term) =
  to_rule_infos_aux
    { name=Gamma(true,mk_name (md name) (mk_ident ("neut_" ^ (string_of_ident (id name)))));
      ctx=[(dloc, (mk_ident "x"),())];
      pat=Pattern (dloc, name,
                   [ Var (dloc,mk_ident "x",0,[]);
                     (* FIXME: Translate term argument to pattern here  *) ]);
      rhs=mk_App (mk_Const dloc name)
                 (mk_DB dloc (mk_ident "x") 0)
                 []
    }

let neu2_rule (name:name) (neu:term) =
  to_rule_infos_aux
    { name=Gamma(true,mk_name (md name) (mk_ident ("neut_" ^ (string_of_ident (id name)))));
      ctx=[(dloc, (mk_ident "x"), ())];
      pat=Pattern (dloc, name,
                   [ Var (dloc,(mk_ident "x"),0,[]) ]);
      rhs=mk_App (mk_Const dloc name)
                 (mk_DB dloc (mk_ident "x") 0)
                 [neu]
    }

let check_confluence_on_import lc (md:mident) (ctx:rw_infos HId.t) : unit =
  let open Confluence in
  let aux id infos =
    let cst = mk_name md id in
    add_constant cst;
    add_rules infos.rules;
    match infos.stat with
    | Definable AC -> add_rules [ comm_rule cst; asso_rule cst ]
    | Definable(ACU neu) ->
      add_rules [ comm_rule cst    ; asso_rule cst;
                  neu1_rule cst neu; neu2_rule cst neu ]
    | _ -> ()
  in
  HId.iter aux ctx;
  Debug.debug d_confluence
    "Checking confluence after loading module '%a'..." pp_mident md;
  try check ()
  with Confluence_error e -> raise (Signature_error (ConfluenceErrorImport (lc,md,e)))

let add_external_declaration sg lc cst scope stat ty =
  try
    let env = HMd.find sg.tables (md cst) in
    if HId.mem env (id cst)
    then raise (Signature_error (AlreadyDefinedSymbol (lc, cst)))
    else HId.replace env (id cst) {stat; ty; scope; rules=[]; decision_tree=None}
  with Not_found ->
    HMd.replace sg.tables (md cst) (HId.create 11);
    let env = HMd.find sg.tables (md cst) in
    HId.replace env (id cst) {stat; ty; scope; rules=[]; decision_tree=None}

(* Recursively load a module and its dependencies*)
let rec import sg lc md =
  if HMd.mem sg.tables md
  then Debug.(debug d_warn "Trying to import the already loaded module %s." (string_of_mident md))
  else
    let (deps,ctx,ext) = unmarshal lc (sg.get_file lc md) in
    HMd.replace sg.tables md ctx;
    List.iter ( fun dep ->
        if not (HMd.mem sg.tables dep) then import sg lc dep
      ) deps ;
    Debug.(debug d_module "Loading module '%a'..." pp_mident md);
    List.iter (fun rs -> add_rule_infos sg rs) ext;
    check_confluence_on_import lc md ctx

and add_rule_infos sg (lst:rule_infos list) : unit =
  match lst with
  | [] -> ()
  | (r::_ as rs) ->
    let infos, env =
      try get_info_env sg r.l r.cst
      with _ when not !fail_on_symbol_not_found ->
        add_external_declaration sg r.l r.cst Public (Definable Free) (mk_Kind);
        get_info_env sg r.l r.cst
    in
    if infos.stat = Static && !fail_on_symbol_not_found
    then raise (Signature_error (CannotAddRewriteRules (r.l,r.cst)));
    HId.replace env (id r.cst) {infos with rules = List.rev_append rs infos.rules; decision_tree= None}
    (* [rs] is in the order the user declared the rules,
       but a [rw_infos] is a Lifo pile, hence the [rev_append]. *)

and compute_dtree sg (lc:Basic.loc) (cst:Basic.name) : Dtree.t =
  let infos, env = get_info_env sg lc cst in
  match infos.decision_tree, infos.rules with
  (* Non-empty set of rule but decision trees not computed *)
  | None, rules ->
    let trees =
      try Dtree.of_rules cst (get_algebra sg dloc) (List.rev rules)
      with Dtree.Dtree_error e -> raise (Signature_error (CannotBuildDtree e))
    in
    HId.replace env (id cst) {infos with decision_tree=Some trees};
    trees
  | Some t, _ -> t

and get_info_env sg lc cst =
  let md = md cst in
  let env =  (* Fetch module, import it if it's missing *)
    try HMd.find sg.tables md
    with Not_found -> import sg lc md; HMd.find sg.tables md
  in
  try (HId.find env (id cst), env)
  with Not_found -> raise (Signature_error (SymbolNotFound (lc,cst)))

and get_infos sg lc cst = fst (get_info_env sg lc cst)

(* and get_type sg lc name = (get_infos sg lc name).ty *)

and get_staticity sg lc name = (get_infos sg lc name).stat

and get_algebra sg lc name = algebra_of_staticity (get_staticity sg lc name)

and is_AC sg lc name = Term.is_AC (get_algebra sg lc name)

(******************************************************************************)

let get_deps sg : mident list = (*only direct dependencies*)
  HMd.fold (
    fun md _ lst ->
      if mident_eq md sg.md then lst
      else md::lst
    ) sg.tables []


let import_signature sg sg_ext =
  HMd.iter (fun m hid ->
      if not (HMd.mem sg.tables m) then
        HMd.replace sg.tables m (HId.copy hid)) sg_ext.tables;
  List.iter (fun rs -> add_rule_infos sg rs) sg_ext.external_rules

let export sg oc =
  let mod_table = HMd.find sg.tables sg.md in
  marshal sg.md (get_deps sg) mod_table sg.external_rules oc

(******************************************************************************)

let is_static sg lc cst =
  match (get_infos sg lc cst).stat with
  | Static                  -> true
  | Definable _ | Injective -> false

let stat_code = function
  | Static            -> 0
  | Definable Free    -> 1
  | Definable AC      -> 2
  | Definable (ACU _) -> 3
  | Injective         -> 4

let get_id_comparator sg cst cst' =
  compare (stat_code (get_staticity sg dloc cst ), cst )
          (stat_code (get_staticity sg dloc cst'), cst')

let get_neutral sg lc cst =
  match get_algebra sg lc cst with
    | ACU neu -> neu
    | _ -> raise (Signature_error (ExpectedACUSymbol(lc,cst)))

let get_env sg lc cst =
  let md = md cst in
  try HMd.find sg.tables md
  with Not_found -> import sg lc md; HMd.find sg.tables md

let get_infos sg lc cst =
  try HId.find (get_env sg lc cst) (id cst)
  with Not_found -> raise (Signature_error (SymbolNotFound (lc,cst)))

let is_injective sg lc cst =
  match (get_infos sg lc cst).stat with
  | Static | Injective -> true
  | Definable _        -> false

let get_type sg lc cst =
  let infos = get_infos sg lc cst in
  if infos.scope = Public || md cst = sg.md
  then infos.ty
  else raise (Signature_error (PrivateSymbol(lc,cst)))

let get_rules sg lc cst = (get_infos sg lc cst).rules

let get_dtree sg lc cst =
  try compute_dtree sg lc cst
  with e -> if not !fail_on_symbol_not_found then Dtree.empty else raise e

(******************************************************************************)

let add_declaration sg lc v st ty =
  let cst = mk_name sg.md v in
  add_external_declaration sg lc cst st ty


let add_rules sg = function
  | [] -> ()
  | r :: _ as rs ->
    try
      add_rule_infos sg rs;
      if not (mident_eq sg.md (md r.cst)) then
        sg.external_rules <- rs::sg.external_rules;
      Confluence.add_rules rs;
      Debug.(debug Confluence.d_confluence
               "Checking confluence after adding rewrite rules on symbol '%a'"
               pp_name r.cst);
      try Confluence.check ()
      with Confluence.Confluence_error e -> raise (Signature_error (ConfluenceErrorRules (r.l,rs,e)))
    with Rule_error e -> raise (Signature_error (CannotMakeRuleInfos e))


let get_rw_infos sg md id =
  if HMd.mem sg.tables md then
    let sig_md = HMd.find sg.tables md in
    if HId.mem sig_md id then
      Some (HId.find sig_md id)
    else
      None
  else
    None
