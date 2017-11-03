(** Global Environment *)

open Basic
open Term
open Rule
open Dtree

let ignore_redecl = ref false

type signature_error =
  | FailToCompileModule of loc * Name.mident
  | UnmarshalBadVersionNumber of loc * string
  | UnmarshalSysError of loc * string * string
  | UnmarshalUnknown of loc * string
  | SymbolNotFound of loc * Name.ident
  | AlreadyDefinedSymbol of loc * Name.ident
  | CannotMakeRuleInfos of Rule.rule_error
  | CannotBuildDtree of Dtree.dtree_error
  | CannotAddRewriteRules of loc * Name.ident
  | ConfluenceErrorImport of loc * Name.mident * Confluence.confluence_error
  | ConfluenceErrorRules of loc * rule_infos list * Confluence.confluence_error

exception SignatureError of signature_error

type dtree_or_def =
  | DoD_None
  | DoD_Def of term
  | DoD_Dtree of int*dtree

module H = Hashtbl.Make(
struct
  type t        = Name.ident
  let equal     = Name.equal
  let hash      = Hashtbl.hash
end )

type staticity = Static | Definable

type rw_infos =
  {
    stat: staticity;
    ty: term;
    rule_opt_info: (rule_infos list*int*dtree) option
  }

type t = { name:Name.mident;
           tables:(rw_infos H.t);}

(* rewrite rules on a definable constant declared in an other module *)
let ext_rules_to_export = ref []

let make name =
  let ht = H.create 251 in
  { name=name; tables=ht}

let get_name sg = sg.name

(******************************************************************************)

let add_rule_infos sg (lst:rule_infos list) : unit =
  match lst with
  | [] -> ()
  | (r::_ as rs) ->
    let infos = try ( H.find sg.tables r.cst)
      with Not_found -> assert false in (* all the dependency should be loaded before *)
    let ty = infos.ty in
    if (infos.stat = Static) then
      raise (SignatureError (CannotAddRewriteRules (r.l,r.cst)));
    let rules = match infos.rule_opt_info with
      | None -> rs
      | Some(mx,_,_) -> mx@rs
    in
    match Dtree.of_rules rules with
    | OK (n,tree) ->
      H.add sg.tables r.cst
        {stat = infos.stat; ty=ty; rule_opt_info = Some(rules,n,tree)}
    | Err e -> raise (SignatureError (CannotBuildDtree e))

(******************************************************************************)

let marshal (name:Name.mident) (deps:string list) (env:rw_infos H.t) : bool =
  try
    begin
      let out = open_out (Name.string_of_mident name ^ ".dko" ) in
        Marshal.to_channel out Version.version [] ;
        Marshal.to_channel out deps [] ;
        Marshal.to_channel out env [] ;
        Marshal.to_channel out !ext_rules_to_export [] ;
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

let unmarshal (lc:loc) (m:string) : string list * rw_infos H.t * rule_infos list list =
  try
    begin
      let chan = find_dko m in
      let ver:string = Marshal.from_channel chan in
        if String.compare ver Version.version = 0 then
          begin
            let deps:string list = Marshal.from_channel chan in
            let ctx:rw_infos H.t = Marshal.from_channel chan in
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

let check_confluence_on_import lc (md:Name.mident) (ctx:rw_infos H.t) : unit =
  let aux cst infos =
    Confluence.add_constant cst;
    match infos.rule_opt_info with
    | None -> ()
    | Some (rs,_,_) -> Confluence.add_rules rs
  in
  H.iter aux ctx;
  debug 1 "Checking confluence after loading module '%a'..." Name.pp_mident md;
  match Confluence.check () with
  | OK () -> ()
  | Err err -> raise (SignatureError (ConfluenceErrorImport (lc,md,err)))

(* Recursively load a module and its dependencies*)
let rec import sg =
  let modules = ref [get_name sg] in
  fun lc m ->
    if List.mem m !modules then
      raise Not_found
    else
    let (deps,ctx,ext) = unmarshal lc (Name.string_of_mident m) in
    List.iter ( fun sdep ->
        let dep = Name.make_mident sdep in
        if not (List.mem dep !modules) then import sg lc dep
      ) deps ;
    debug 1 "Loading module '%a'..." Name.pp_mident m;
    List.iter (fun rs -> add_rule_infos sg rs) ext;
    check_confluence_on_import lc m ctx

let get_deps sg : string list = (*only direct dependencies*)
  H.fold (
    fun cst _ lst ->
      let md = Name.md cst in
      if Name.mequal md sg.name then lst
      else (Name.string_of_mident md)::lst
    ) sg.tables []

let export sg =
  marshal sg.name (get_deps sg) sg.tables

(******************************************************************************)

let get_infos sg lc cst =
  debug 3 "Looking for symbol '%a'" Name.pp_ident cst;
  try H.find sg.tables cst
  with Not_found ->
  try
    import sg lc (Name.md cst);
    H.find sg.tables cst
  with Not_found -> raise (SignatureError (SymbolNotFound (lc,cst)))

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

let add_declaration sg lc cst st ty =
  Confluence.add_constant cst;
  debug 2 "Adding declaration '%a'" Name.pp_ident cst;
  if H.mem sg.tables cst then
    ( if !ignore_redecl then debug 1 "Redeclaration ignored."
      else raise (SignatureError (AlreadyDefinedSymbol (lc,cst))) )
  else
    begin
      debug 2 "WTF";
      H.add sg.tables cst {stat=st; ty=ty; rule_opt_info=None}
    end

let add_rules sg lst : unit =
  let rs = map_error_list Rule.to_rule_infos lst in
  match rs with
  | Err e -> raise (SignatureError (CannotMakeRuleInfos e))
  | OK [] -> ()
  | OK (r::_ as rs) ->
    begin
      add_rule_infos sg rs;
      if not (Name.mequal sg.name (Name.md r.cst)) then
        ext_rules_to_export := rs::!ext_rules_to_export;
      Confluence.add_rules rs;
      debug 3 "Checking confluence after adding rewrite rules on symbol '%a'"
        Name.pp_ident r.cst;
      match Confluence.check () with
      | OK () -> ()
      | Err err -> raise (SignatureError (ConfluenceErrorRules (r.l,rs,err)))
    end
