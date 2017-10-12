(** Global Environment *)

open Basic
open Term
open Rule
open Dtree

let ignore_redecl = ref false
let autodep = ref false

type signature_error =
  | FailToCompileModule of loc*ident
  | UnmarshalBadVersionNumber of loc*string
  | UnmarshalSysError of loc*string*string
  | UnmarshalUnknown of loc*string
  | SymbolNotFound of loc*ident*ident
  | AlreadyDefinedSymbol of loc*ident
  | CannotMakeRuleInfos of rule_error
  | CannotBuildDtree of dtree_error
  | CannotAddRewriteRules of loc*ident
  | ConfluenceErrorImport of loc*ident*Confluence.confluence_error
  | ConfluenceErrorRules of loc*rule_infos list*Confluence.confluence_error

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

type staticity = Static | Definable | DefinableAC | DefinableACU of term

type rw_infos =
  {
    stat: staticity;
    ty: term;
    rule_opt_info: (rule_infos list*dtree) option
  }

type t = { name:ident; tables:(rw_infos H.t) H.t;
           mutable external_rules:rule_infos list list; }

let make name =
  let ht = H.create 19 in
  H.add ht name (H.create 251); { name=name; tables=ht; external_rules=[]; }

let get_name sg = sg.name

(******************************************************************************)

let add_rule_infos sg (lst:rule_infos list) : unit =
  match lst with
  | [] -> ()
  | (r::_ as rs) ->
    let env =
      try H.find sg.tables r.md
      with Not_found -> assert false in (*should not happen if the dependencies are loaded before*)
    let infos = try ( H.find env r.id )
      with Not_found -> assert false in
    let ty = infos.ty in
    if (infos.stat = Static) then
      raise (SignatureError (CannotAddRewriteRules (r.l,r.id)));
    let rules = match infos.rule_opt_info with
      | None -> rs
      | Some(mx,_) -> mx@rs
    in
    match Dtree.of_rules rules with
    | OK tree ->
       H.add env r.id
         {stat = infos.stat; ty=ty; rule_opt_info = Some(rules,tree)}
    | Err e -> raise (SignatureError (CannotBuildDtree e))

(******************************************************************************)

let marshal (name:ident) (deps:string list) (env:rw_infos H.t) (ext:rule_infos list list) : bool =
  try
    begin
      let out = open_out (string_of_ident name ^ ".dko" ) in
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

let get_type_from_AC (ty:term) =
  match ty with
  | Pi(_,_,t,_) -> t
  | _ -> assert false

let to_rule_infos_aux (r:typed_rule) =
  match Rule.to_rule_infos r with
  | Err e -> raise (SignatureError (CannotMakeRuleInfos e))
  | OK  e -> e

let comm_rule (md:ident) (id:ident) (ty:term) =
  to_rule_infos_aux
    { name=Gamma(true,md,(hstring ("comm_" ^ (string_of_ident id))));
      ctx=[(dloc, (hstring "x"), ty); (dloc,(hstring "y"), ty)];
      pat=Pattern (dloc, md, id,
                   [ Var (dloc,(hstring "x"),0,[]);
                     Var (dloc,(hstring "y"),1,[]) ]);
      rhs=mk_App (mk_Const dloc md id)
                 (mk_DB dloc (hstring "y") 1)
                 [mk_DB dloc (hstring "x") 0]
    }

(* FIXME *)
let asso_rule (md:ident) (id:ident) (ty:term) =
  to_rule_infos_aux
    { name=Gamma(true,md,(hstring ("asso_" ^ (string_of_ident id))));
      ctx=[ (dloc, (hstring "x"), ty);
            (dloc, (hstring "y"), ty);
            (dloc, (hstring "z"), ty) ];
      pat=Pattern (dloc, md, id,
                   [ Pattern (dloc, md, id,
                              [ Var (dloc,(hstring "x"),0,[]);
                                Var (dloc,(hstring "y"),1,[]) ] );
                     Var (dloc,(hstring "z"),2,[]) ] );
      rhs=mk_App (mk_Const dloc md id)
                 (mk_DB dloc (hstring "x") 0)
                 [mk_App (mk_Const dloc md id)
                         (mk_DB dloc (hstring "y") 1)
                         [(mk_DB dloc (hstring "z") 2)] ]
    }
    
let neu1_rule (md:ident) (id:ident) (ty:term) (neu:term) =
  to_rule_infos_aux
    { name=Gamma(true,md,(hstring ("neut_" ^ (string_of_ident id))));
      ctx=[(dloc, (hstring "x"), ty)];
      pat=Pattern (dloc, md, id,
                   [ Var (dloc,(hstring "x"),0,[]);
(* FIXME: Translate term neu to pattern here  *) ]);
      rhs=mk_App (mk_Const dloc md id)
                 (mk_DB dloc (hstring "x") 0)
                 []
    }

let neu2_rule (md:ident) (id:ident) (ty:term) (neu:term) =
  to_rule_infos_aux
    { name=Gamma(true,md,(hstring ("neut_" ^ (string_of_ident id))));
      ctx=[(dloc, (hstring "x"), ty)];
      pat=Pattern (dloc, md, id,
                   [ Var (dloc,(hstring "x"),0,[]) ]);
      rhs=mk_App (mk_Const dloc md id)
                 (mk_DB dloc (hstring "x") 0)
                 [neu]
    }

let check_confluence_on_import lc (md:ident) (ctx:rw_infos H.t) : unit =
  let aux id infos =
    Confluence.add_constant md id;
    match infos.rule_opt_info with
    | None -> ()
    | Some (rs,_) -> Confluence.add_rules rs;
    match infos.stat with
    | DefinableAC ->
       let ty = get_type_from_AC infos.ty in
       Confluence.add_rules [ comm_rule md id ty; asso_rule md id ty ]
    | DefinableACU neu ->
       let ty = get_type_from_AC infos.ty in
       Confluence.add_rules [ comm_rule md id ty; asso_rule md id ty;
                              neu1_rule md id ty neu; neu2_rule md id ty neu ]
    | _ -> ()
  in
  H.iter aux ctx;
  debug 1 "Checking confluence after loading module '%a'..." pp_ident md;
  match Confluence.check () with
  | OK () -> ()
  | Err err -> raise (SignatureError (ConfluenceErrorImport (lc,md,err)))

(* Recursively load a module and its dependencies*)
let rec import sg lc m =
  assert ( not (H.mem sg.tables m) ) ;

  (* If the [.dko] file is not found, try to compile it first.
     This hack is terrible. It uses system calls and can loop with circular dependencies.
     Also, this hack supposes that the module name and the file name are the same.*)
  ( if !autodep && not ( Sys.file_exists ( string_of_ident m ^ ".dko" ) ) then
      if Sys.command ( "dkcheck -autodep -e " ^ string_of_ident m ^ ".dk" ) <> 0 then
        raise (SignatureError (FailToCompileModule (lc,m)))
  ) ;

  let (deps,ctx,ext) = unmarshal lc (string_of_ident m) in
  H.add sg.tables m ctx;
  List.iter ( fun dep0 ->
      let dep = hstring dep0 in
      if not (H.mem sg.tables dep) then ignore (import sg lc dep)
    ) deps ;
  debug 1 "Loading module '%a'..." pp_ident m;
  List.iter (fun rs -> add_rule_infos sg rs) ext;
  check_confluence_on_import lc m ctx;
  ctx

let get_deps sg : string list = (*only direct dependencies*)
  H.fold (
    fun md _ lst ->
      if ident_eq md sg.name then lst
      else (string_of_ident md)::lst
    ) sg.tables []

let export sg =
  marshal sg.name (get_deps sg) (H.find sg.tables sg.name) sg.external_rules

(******************************************************************************)

let get_infos sg lc m v =
  let env =
    try H.find sg.tables m
    with Not_found -> import sg lc m
  in
    try ( H.find env v )
    with Not_found -> raise (SignatureError (SymbolNotFound (lc,m,v)))

let get_type sg lc m v = (get_infos sg lc m v).ty

let is_injective sg lc m v =
  match (get_infos sg lc m v).stat with
    | Static -> true
    | _ -> false

let pred_true: Rule.rule_name -> bool = fun x -> true

let get_dtree sg ?select:(pred=pred_true) l m v =
  match (get_infos sg l m v).rule_opt_info with
  | None -> None
  | Some(rules,tr) ->
    if pred == pred_true then
      Some tr
    else
      let rules' = List.filter (fun (r:Rule.rule_infos) -> pred r.name) rules in
      if List.length rules' == List.length rules
      then Some tr
      else
        (* A call to Dtree.of_rules must be made with a non-empty list *)
        match rules' with
        | [] -> None
        | _ -> match Dtree.of_rules rules' with
               | OK tree -> Some tree
               | Err e -> raise (SignatureError (CannotBuildDtree e))


(******************************************************************************)

let add_declaration sg lc v st ty =
  Confluence.add_constant sg.name v;
  let env = H.find sg.tables sg.name in
  if H.mem env v then
    ( if !ignore_redecl then debug 1 "Redeclaration ignored."
      else raise (SignatureError (AlreadyDefinedSymbol (lc,v))) )
  else
    H.add env v {stat=st; ty=ty; rule_opt_info=None}

let add_rules sg lst : unit =
  let rs = map_error_list Rule.to_rule_infos lst in
  match rs with
  | Err e -> raise (SignatureError (CannotMakeRuleInfos e))
  | OK [] -> ()
  | OK (r::_ as rs) ->
    begin
      add_rule_infos sg rs;
      if not (ident_eq sg.name r.md) then
        sg.external_rules <- rs::sg.external_rules;
      Confluence.add_rules rs;
      debug 1 "Checking confluence after adding rewrite rules on symbol '%a.%a' test"
            pp_ident r.md pp_ident r.id;
      match Confluence.check () with
      | OK () -> ()
      | Err err -> raise (SignatureError (ConfluenceErrorRules (r.l,rs,err)))
    end
