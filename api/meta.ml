open Kernel
open Parsers

module type ENCODING = sig
  val md : Basic.mident

  val entries : unit -> Entry.entry list

  val safe : bool

  val signature : Signature.t

  val encode_term :
    ?sg:Signature.t -> ?ctx:Term.typed_context -> Term.term -> Term.term

  val decode_term : Term.term -> Term.term

  val encode_rule : ?sg:Signature.t -> 'a Rule.rule -> 'a Rule.rule
end

module RNS = Set.Make (struct
  type t = Rule.rule_name

  let compare = compare
end)

type cfg = {
  mutable meta_rules : RNS.t list option;
  (* Set of meta_rules used to normalize *)
  beta : bool;
  (* Allows beta doing normalization *)
  register_before : bool;
  (* entries are registered before they have been normalized *)
  encode_meta_rules : bool;
  (* The encoding is used on the meta rules first except for products *)
  encoding : (module ENCODING) option;
  (* Encoding specify a quoting mechanism *)
  decoding : bool;
  (* If false, the term is not decoded after normalization *)
  env : Env.t; (* The current environment (if the encoding needs type checking *)
}

let default_config =
  {
    meta_rules = None;
    beta = true;
    encoding = None;
    register_before = true;
    encode_meta_rules = false;
    env = Env.init (Parser.input_from_string (Basic.mk_mident "") "");
    decoding = true;
  }

let unsafe_finder sg l name =
  try Kernel.Signature.get_dtree sg l name with _ -> Dtree.empty

(* A dkmeta configuration to a reduction configuration *)
let red_cfg : cfg -> Reduction.red_cfg list =
 fun cfg ->
  let open Reduction in
  let red_cfg =
    {
      default_cfg with
      beta = cfg.beta;
      target = Snf;
      finder = unsafe_finder;
      strat = ByValue;
    }
  in
  match cfg.meta_rules with
  | None   -> [{red_cfg with select = Some (fun _ -> true)}]
  | Some l ->
      List.map
        (fun meta_rules ->
          {red_cfg with select = Some (fun r -> RNS.mem r meta_rules)})
        l

module PROD = struct
  open Basic
  open Term

  let md = mk_mident "prod"

  let entries () =
    let mk_decl id =
      Entry.Decl
        ( dloc,
          mk_ident id,
          Signature.Public,
          Signature.Definable Free,
          mk_Type dloc )
    in
    List.map mk_decl ["ty"; "prod"]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Public
        (Signature.Definable Free) (mk_Type dloc)
    in
    List.iter mk_decl ["ty"; "prod"];
    sg

  let safe = false

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec encode_term t =
    match t with
    | Kind                 -> assert false
    | Type lc              -> encode_type lc
    | DB (lc, x, n)        -> encode_DB lc x n
    | Const (lc, name)     -> encode_Const lc name
    | Lam (lc, x, mty, te) -> encode_Lam lc x mty te
    | App (f, a, args)     -> encode_App f a args
    | Pi (lc, x, a, b)     -> encode_Pi lc x a b

  and encode_type lc = mk_Const lc (name_of "ty")

  and encode_DB lc x n = mk_DB lc x n

  and encode_Const _ name = mk_Const dloc name

  and encode_Lam _ x mty te =
    let mty' =
      match mty with None -> None | Some ty -> Some (encode_term ty)
    in
    mk_Lam dloc x mty' (encode_term te)

  and encode_App f a args =
    mk_App2 (encode_term f) (List.map encode_term (a :: args))

  and encode_Pi _ x a b =
    mk_App (const_of "prod") (encode_term a)
      [mk_Lam dloc x (Some (encode_term a)) (encode_term b)]

  (* Using typed context here does not make sense *)
  let encode_pattern pattern : Rule.pattern = pattern

  let encode_rule (r : 'a Rule.rule) =
    let open Rule in
    {r with pat = encode_pattern r.pat; rhs = encode_term r.rhs}

  let encode_term ?sg:_ ?ctx:_ t = encode_term t

  let encode_rule ?sg:_ r = encode_rule r

  let rec decode_term t =
    match t with
    | Kind                 -> assert false
    | Pi _                 -> assert false
    | App (f, a, args)     -> decode_App f a args
    | Lam (lc, x, mty, te) -> decode_Lam lc x mty te
    | Const (lc, name)     -> decode_Const lc name
    | Type _ | DB _        -> t

  and decode_Const lc name =
    if name_eq name (name_of "ty") then mk_Type lc else mk_Const lc name

  and decode_Lam lc x mty te =
    let mty' =
      match mty with None -> None | Some mty -> Some (decode_term mty)
    in
    mk_Lam lc x mty' (decode_term te)

  and decode_App f a args =
    match f with
    | Const (_, name) ->
        if name_eq name (name_of "prod") then
          match args with
          | [Lam (_, x, Some a, b)] ->
              mk_Pi dloc x (decode_term a) (decode_term b)
          | _                       -> assert false
        else mk_App (decode_term f) (decode_term a) (List.map decode_term args)
    | _               -> mk_App (decode_term f) (decode_term a)
                           (List.map decode_term args)
end

module LF = struct
  open Basic
  open Term

  let md = mk_mident "lf"

  let entries () =
    let mk_decl id =
      Entry.Decl
        ( dloc,
          mk_ident id,
          Signature.Public,
          Signature.Definable Free,
          mk_Type dloc )
    in
    List.map mk_decl ["ty"; "var"; "sym"; "lam"; "app"; "prod"]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Public
        (Signature.Definable Free) (mk_Type dloc)
    in
    List.iter mk_decl ["ty"; "var"; "sym"; "lam"; "app"; "prod"];
    sg

  let safe = false

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec encode_term t =
    match t with
    | Kind                 -> assert false
    | Type lc              -> encode_type lc
    | DB (lc, x, n)        -> encode_DB lc x n
    | Const (lc, name)     -> encode_Const lc name
    | Lam (lc, x, mty, te) -> encode_Lam lc x mty te
    | App (f, a, args)     -> encode_App f a args
    | Pi (lc, x, a, b)     -> encode_Pi lc x a b

  and encode_type _ = const_of "ty"

  and encode_DB lc x n = mk_App (const_of "var") (mk_DB lc x n) []

  and encode_Const _ name = mk_App (const_of "sym") (mk_Const dloc name) []

  and encode_Lam _ x mty te =
    let mty' =
      match mty with None -> None | Some ty -> Some (encode_term ty)
    in
    mk_App (const_of "lam") (mk_Lam dloc x mty' (encode_term te)) []

  and encode_App f a args =
    let rec encode_app2 a args =
      match (a, args) with
      | _, []     -> assert false
      | a, [x]    -> mk_App (const_of "app") a [encode_term x]
      | a, x :: l -> encode_app2 (mk_App (const_of "app") a [encode_term x]) l
    in
    encode_app2 (encode_term f) (a :: args)

  and encode_Pi _ x a b =
    mk_App (const_of "prod") (encode_term a)
      [mk_Lam dloc x None (encode_term b)]

  (* Using typed context here does not make sense *)
  let rec encode_pattern pattern : Rule.pattern =
    let open Rule in
    match pattern with
    | Var (lc, id, n, ps) -> Var (lc, id, n, List.map encode_pattern ps)
    | Brackets term       -> Brackets (encode_term term)
    | Lambda (lc, id, p)  ->
        Pattern (lc, name_of "lam", [Lambda (lc, id, encode_pattern p)])
    | Pattern (lc, n, []) -> Pattern (lc, name_of "sym", [Pattern (lc, n, [])])
    | Pattern (lc, n, ps) ->
        Pattern
          ( lc,
            name_of "app",
            Pattern (lc, name_of "sym", [Pattern (lc, n, [])])
            :: List.map encode_pattern ps )

  let encode_rule (r : 'a Rule.rule) =
    let open Rule in
    {r with pat = encode_pattern r.pat; rhs = encode_term r.rhs}

  let encode_term ?sg:_ ?ctx:_ t = encode_term t

  let encode_rule ?sg:_ r = encode_rule r

  let rec decode_term t =
    match t with
    | Kind                 -> assert false
    | Type _               -> assert false
    | DB (lc, x, n)        -> decode_DB lc x n
    | Const (lc, name)     -> decode_Const lc name
    | Lam (lc, x, mty, te) -> decode_Lam lc x mty te
    | App (f, a, args)     -> decode_App f a args
    | Pi (lc, x, a, b)     -> decode_Pi lc x a b

  and decode_DB lc x n = mk_DB lc x n

  and decode_Const lc name =
    if name_eq name (name_of "ty") then mk_Type dloc else mk_Const lc name

  and decode_Lam lc x mty te =
    let mty' =
      match mty with None -> None | Some mty -> Some (decode_term mty)
    in
    mk_Lam lc x mty' (decode_term te)

  and decode_App f a args =
    match f with
    | Const (_, name) ->
        if name_eq name (name_of "prod") then
          match (a, args) with
          | a, [Lam (_, x, None, b)] ->
              mk_Pi dloc x (decode_term a) (decode_term b)
          | _                        -> assert false
        else if name_eq name (name_of "sym") then decode_term a
        else if name_eq name (name_of "var") then decode_term a
        else if name_eq name (name_of "app") then
          mk_App2 (decode_term a) (List.map decode_term args)
        else if name_eq name (name_of "lam") then decode_term a
        else mk_App (decode_term f) (decode_term a) (List.map decode_term args)
    | _               ->
        decode_App (decode_term f) (decode_term a) (List.map decode_term args)

  and decode_Pi _ _ _ _ = assert false
end

module APP = struct
  open Basic
  open Term

  let md = mk_mident "ltyped"

  let entries () =
    let mk_decl id =
      Entry.Decl
        ( dloc,
          mk_ident id,
          Signature.Public,
          Signature.Definable Free,
          mk_Type dloc )
    in
    List.map mk_decl ["ty"; "var"; "sym"; "lam"; "app"; "prod"]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Public
        (Signature.Definable Free) (mk_Type dloc)
    in
    List.iter mk_decl ["ty"; "var"; "sym"; "lam"; "app"; "prod"];
    sg

  let safe = true

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec encode_term sg ctx t =
    match t with
    | Kind                     -> assert false
    | Type lc                  -> encode_type sg ctx lc
    | DB (lc, x, n)            -> encode_DB sg ctx lc x n
    | Const (lc, name)         -> encode_Const sg ctx lc name
    | Lam (lc, x, Some ty, te) -> encode_Lam sg ctx lc x ty te
    | Lam _                    -> assert false
    | App (f, a, args)         -> encode_App sg ctx f a args
    | Pi (lc, x, a, b)         -> encode_Pi sg ctx lc x a b

  and encode_type _ _ _ = const_of "ty"

  and encode_DB _ _ lc x n = mk_App (const_of "var") (mk_DB lc x n) []

  and encode_Const _ _ _ name = mk_App (const_of "sym") (mk_Const dloc name) []

  and encode_Lam sg ctx lc x ty te =
    let ctx' = (lc, x, ty) :: ctx in
    let tyf = Typing.Default.infer sg ctx (mk_Lam lc x (Some ty) te) in
    let tyf' = PROD.encode_term tyf in
    mk_App (const_of "lam") tyf'
      [mk_Lam dloc x (Some (encode_term sg ctx ty)) (encode_term sg ctx' te)]

  and encode_App sg ctx f a args = encode_app2 sg ctx f (a :: args)

  and encode_app2 sg ctx f args =
    let aux f f' a =
      let tyf = Typing.Default.infer sg ctx f in
      let tyf' = PROD.encode_term tyf in
      ( Term.mk_App2 f [a],
        mk_App (const_of "app") tyf' [f'; encode_term sg ctx a] )
    in
    snd
    @@ List.fold_left
         (fun (f, f') a -> aux f f' a)
         (f, encode_term sg ctx f)
         args

  and encode_Pi sg ctx lc x a b =
    let ctx' = (lc, x, a) :: ctx in
    mk_App (const_of "prod")
      (mk_Lam dloc x (Some (encode_term sg ctx a)) (encode_term sg ctx' b))
      []

  let rec encode_pattern sg ctx pattern =
    let open Rule in
    let dummy = Var (Basic.dloc, mk_ident "_", 0, []) in
    let mk_pat_app l r =
      Pattern (Basic.dloc, name_of "app", [dummy; l; encode_pattern sg ctx r])
    in
    match pattern with
    | Var (lc, id, n, ps)     -> Var
                                   ( lc,
                                     id,
                                     n,
                                     List.map (encode_pattern sg ctx) ps )
    | Brackets term           -> Brackets (encode_term sg ctx term)
    | Lambda (lc, id, p)      ->
        Pattern
          (lc, name_of "lam", [dummy; Lambda (lc, id, encode_pattern sg ctx p)])
    | Pattern (lc, n, [])     -> Pattern
                                   (lc, name_of "sym", [Pattern (lc, n, [])])
    | Pattern (lc, n, [a])    ->
        Pattern
          ( lc,
            name_of "app",
            [
              dummy;
              Pattern (lc, name_of "sym", [Pattern (lc, n, [])]);
              encode_pattern sg ctx a;
            ] )
    | Pattern (lc, n, a :: l) ->
        List.fold_left
          (fun p arg -> mk_pat_app p arg)
          (encode_pattern sg ctx (Pattern (lc, n, [])))
          (a :: l)

  let encode_rule sg r =
    let r' = Rule.untyped_rule_of_rule_infos (Rule.to_rule_infos r) in
    let _, r'' = Typing.Default.check_rule sg r' in
    let open Rule in
    {
      r with
      pat = encode_pattern sg r''.ctx r.pat;
      rhs = encode_term sg r''.ctx r.rhs;
    }

  let fake_sig () = Signature.make (Basic.mk_mident "") Files.find_object_file

  let encode_term ?(sg = fake_sig ()) ?(ctx = []) t = encode_term sg ctx t

  let encode_rule ?(sg = fake_sig ()) r = encode_rule sg r

  let rec decode_term t =
    match t with
    | Kind                 -> assert false
    | Type _               -> assert false
    | DB (lc, x, n)        -> decode_DB lc x n
    | Const (lc, name)     -> decode_Const lc name
    | Lam (lc, x, mty, te) -> decode_Lam lc x mty te
    | App (f, a, args)     -> decode_App f a args
    | Pi (lc, x, a, b)     -> decode_Pi lc x a b

  and decode_DB lc x n = mk_DB lc x n

  and decode_Const lc name =
    if name_eq name (name_of "ty") then mk_Type dloc else mk_Const lc name

  and decode_Lam lc x mty te =
    let mty' =
      match mty with None -> None | Some mty -> Some (decode_term mty)
    in
    mk_Lam lc x mty' (decode_term te)

  and decode_App f a args =
    match f with
    | Const (_, name) ->
        if name_eq name (name_of "prod") then
          match a with
          | Lam (_, x, Some a, b) ->
              mk_Pi dloc x (decode_term a) (decode_term b)
          | _                     -> assert false
        else if name_eq name (name_of "sym") then decode_term a
        else if name_eq name (name_of "var") then decode_term a
        else if name_eq name (name_of "app") then
          match args with
          | [f; a] -> Term.mk_App2 (decode_term f) [decode_term a]
          | _      -> assert false
        else if name_eq name (name_of "lam") then
          match args with [a] -> decode_term a | _ -> assert false
        else mk_App (decode_term f) (decode_term a) (List.map decode_term args)
    | _               -> mk_App (decode_term f) (decode_term a)
                           (List.map decode_term args)

  and decode_Pi _ _ _ _ = assert false
end

let encode sg cfg term =
  match cfg.encoding with
  | None                       -> term
  | Some (module E : ENCODING) ->
      if E.safe then E.encode_term ~sg term else E.encode_term term

let decode cfg term =
  match cfg.encoding with
  | None                       -> term
  | Some (module E : ENCODING) -> E.decode_term term

let normalize cfg term =
  let reds = red_cfg cfg in
  let sg = Env.get_signature cfg.env in
  List.fold_left
    (fun term red -> Reduction.Default.reduction red sg term)
    term reds

let mk_term cfg ?(env = cfg.env) term =
  (* Format.eprintf "b:%a@." Pp.print_term term; *)
  let sg = Env.get_signature env in
  let term' = encode sg cfg term in
  let term'' = normalize cfg term' in
  if cfg.decoding then decode cfg term'' else term''

exception Not_a_pattern

let rec pattern_of_term t =
  let open Term in
  match t with
  | Kind | Type _ | Pi _ -> raise Not_a_pattern
  | Lam (lc, x, _, te) -> Rule.Lambda (lc, x, pattern_of_term te)
  | App (Const (lc, name), a, args) ->
      Rule.Pattern (lc, name, List.map pattern_of_term (a :: args))
  | App (DB (lc, x, n), a, args) ->
      Rule.Var (lc, x, n, List.map pattern_of_term (a :: args))
  | Const (lc, name) -> Rule.Pattern (lc, name, [])
  | DB (lc, x, n) -> Rule.Var (lc, x, n, [])
  | _ -> raise Not_a_pattern

let mk_rule env cfg (r : Rule.partially_typed_rule) =
  let open Rule in
  match cfg.encoding with
  | None                       -> {r with rhs = normalize cfg r.rhs}
  | Some (module E : ENCODING) ->
      let sg = Env.get_signature env in
      let r' = E.encode_rule ~sg r in
      let pat' = normalize cfg (Rule.pattern_to_term r'.pat) in
      let pat'' =
        if cfg.decoding then pattern_of_term (E.decode_term pat')
        else pattern_of_term pat'
      in
      let rhs' = normalize cfg r'.rhs in
      let rhs'' = if cfg.decoding then decode cfg rhs' else rhs' in
      {pat = pat''; rhs = rhs''; ctx = r.ctx; name = r.name}

(*
  let t = Rule.pattern_to_term pat in
  let t' = mk_term cfg t in
  pattern_of_term t' *)

module D = Basic.Debug

let debug_flag = D.register_flag "Dkmeta"

let bmag fmt = "\027[90m" ^^ fmt ^^ "\027[0m%!"

let log fmt = D.debug debug_flag (bmag fmt)

let mk_entry env cfg entry =
  let open Entry in
  let open Rule in
  let cfg = {cfg with env} in
  let sg = Env.get_signature env in
  let md = Env.get_name env in
  match entry with
  | Decl (lc, id, sc, st, ty) ->
      log "[NORMALIZE] %a" Basic.pp_ident id;
      let ty' = mk_term cfg ~env ty in
      if cfg.register_before then Signature.add_declaration sg lc id sc st ty
      else Signature.add_declaration sg lc id sc st ty';
      Decl (lc, id, sc, st, ty')
  | Def (lc, id, sc, opaque, ty, te) -> (
      log "[NORMALIZE] %a" Basic.pp_ident id;
      let cst = Basic.mk_name md id in
      let rule =
        {name = Delta cst; ctx = []; pat = Pattern (lc, cst, []); rhs = te}
      in
      let safe_ty =
        match (cfg.encoding, ty) with
        | Some (module E : ENCODING), None when E.safe -> Env.infer cfg.env te
        | _, Some ty -> ty
        | _, _ -> Term.mk_Type Basic.dloc
      in
      let safe_ty' = mk_term cfg ~env safe_ty in
      let te' = mk_term cfg ~env te in
      (if cfg.register_before then
       let _ =
         Signature.add_declaration sg lc id sc (Signature.Definable Free)
           safe_ty
       in
       Signature.add_rules sg
         (List.map Rule.to_rule_infos [{rule with rhs = te}])
      else
        let _ =
          Signature.add_declaration sg lc id sc (Signature.Definable Free)
            safe_ty'
        in
        Signature.add_rules sg
          (List.map Rule.to_rule_infos [{rule with rhs = te'}]));
      match ty with
      | None   -> Def (lc, id, sc, opaque, None, te')
      | Some _ -> Def (lc, id, sc, opaque, Some safe_ty', te'))
  | Rules (lc, rs) ->
      (* Signature.add_rules !sg (List.map Rule.to_rule_infos rs); *)
      let rs' = List.map (mk_rule env cfg) rs in
      if cfg.register_before then
        Signature.add_rules sg (List.map Rule.to_rule_infos rs)
      else Signature.add_rules sg (List.map Rule.to_rule_infos rs');
      Rules (lc, rs')
  | _ -> entry

let add_rule sg r = Signature.add_rules sg [Rule.to_rule_infos r]

(* Several rules might be bound to different constants *)
let add_rules sg rs = List.iter (add_rule sg) rs

let meta_of_rules : ?staged:bool -> Rule.partially_typed_rule list -> cfg -> cfg
    =
 fun ?(staged = false) rules cfg ->
  let rule_names =
    List.map (fun (r : Rule.partially_typed_rule) -> r.Rule.name) rules
  in
  let sg = Env.get_signature cfg.env in
  add_rules sg rules;
  match cfg.meta_rules with
  | None           -> {cfg with meta_rules = Some [RNS.of_list rule_names]}
  | Some []        -> assert false
  | Some (rs :: l) ->
      if staged then
        {cfg with meta_rules = Some (RNS.of_list rule_names :: rs :: l)}
      else
        {
          cfg with
          meta_rules = Some (RNS.union (RNS.of_list rule_names) rs :: l);
        }

module MetaConfiguration :
  Processor.S with type t = Rule.partially_typed_rule list = struct
  type t = Rule.partially_typed_rule list

  let rules = ref []

  let handle_entry _ = function
    | Entry.Rules (_, rs) -> rules := rs :: !rules
    (* TODO: Handle definitions *)
    | _ -> ()

  let get_data _ =
    let rs = List.flatten !rules in
    rules := [];
    rs
end

type _ Processor.t += MetaRules : Rule.partially_typed_rule list Processor.t

let _ =
  let equal (type a b) :
      a Processor.t * b Processor.t ->
      (a Processor.t, b Processor.t) Processor.Registration.equal option =
    function
    | MetaRules, MetaRules -> Some (Processor.Registration.Refl MetaRules)
    | _                    -> None
  in
  Processor.Registration.register_processor MetaRules {equal}
    (module MetaConfiguration)

let meta_of_files ?(cfg = default_config) files =
  Processor.fold_files files
    ~f:(meta_of_rules ~staged:true)
    ~default:cfg MetaRules

let make_meta_processor cfg ~post_processing =
  let module Meta = struct
    type t = unit

    let handle_entry env entry = post_processing env (mk_entry env cfg entry)

    let get_data _ = ()
  end in
  (module Meta : Processor.S with type t = unit)
