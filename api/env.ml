open Kernel
open Basic
open Term
open Rule
open Typing
open Signature
open Parsers

exception DebugFlagNotRecognized of char

let set_debug_mode =
  String.iter (function
      | 'q' -> Debug.disable_flag Debug.D_warn
      | 'n' -> Debug.enable_flag  Debug.D_notice
      | 'o' -> Debug.enable_flag  Signature.D_module
      | 'c' -> Debug.enable_flag  Confluence.D_confluence
      | 'u' -> Debug.enable_flag  Typing.D_rule
      | 't' -> Debug.enable_flag  Typing.D_typeChecking
      | 'r' -> Debug.enable_flag  Reduction.D_reduce
      | 'm' -> Debug.enable_flag  Dtree.D_matching
      | c -> raise (DebugFlagNotRecognized c)
    )

let raise_as_env md lc = function
  | Signature_error e -> raise (Entry.EnvError (Some md, lc, (EnvErrorSignature e)))
  | Typing_error    e -> raise (Entry.EnvError (Some md, lc, (EnvErrorType      e)))
  | Rule_error      e -> raise (Entry.EnvError (Some md, lc, (EnvErrorRule      e)))
  | ex               -> raise ex

let check_arity = ref true

let check_ll = ref false

module type S =
sig
  module Printer : Pp.Printer
  val raise_env : loc -> Entry.env_error -> 'a

  val init        : string -> mident

  val get_signature : unit -> Signature.t
  val get_name    : unit -> mident
  module HName : Hashtbl.S with type key = name
  val get_symbols : unit -> Signature.rw_infos HName.t
  val get_type    : loc -> name -> term
  val is_static   : loc -> name -> bool
  val get_dtree   : loc -> name -> Dtree.t
  val export      : unit -> unit
  val import      : loc -> mident -> unit
  val declare     : loc -> ident -> Signature.staticity -> term -> unit
  val define      : loc -> ident -> bool -> term -> term option -> unit
  val add_rules   : Rule.untyped_rule list -> (Subst.Subst.t * Rule.typed_rule) list

  val infer            : ?ctx:typed_context -> term         -> term
  val check            : ?ctx:typed_context -> term -> term -> unit
  val reduction        : ?ctx:typed_context -> ?red:(Reduction.red_cfg) -> term -> term
  val are_convertible  : ?ctx:typed_context -> term -> term -> bool
  val unsafe_reduction : ?red:(Reduction.red_cfg) -> term -> term

end

(* Wrapper around Signature *)
module Make(R:Reduction.S) =
struct
  module T = Typing.Make(R)

  let sg =
    let md = Basic.mk_mident "noname" in
    ref (Signature.make md Dep.get_file)

  let current_file = ref "noname"

  let md_of_file file =
    let open Filename in
    let base = basename file in
    let base = if check_suffix base ".dk" then (chop_suffix base ".dk") else base in
    mk_mident base

  let init file =
    current_file := file;
    sg := Signature.make (md_of_file file) Dep.get_file;
    Signature.get_name !sg

  let get_name () = Signature.get_name !sg

  let get_signature () = !sg

  let raise_as_env x = raise_as_env (get_name()) x
  let raise_env lc err = raise (Entry.EnvError (Some (get_name()), lc, err))

  module Printer = Pp.Make(struct let get_name = get_name end)

  module HName = Hashtbl.Make(
    struct
      type t    = name
      let equal = name_eq
      let hash  = Hashtbl.hash
    end )

  let get_symbols () =
    let table = HName.create 11 in
    Signature.iter_symbols (fun md id -> HName.add table (mk_name md id)) !sg;
    table

  let get_type lc cst =
    try Signature.get_type !sg lc cst
    with e -> raise_as_env lc e

  let get_dtree lc cst =
    try Signature.get_dtree !sg lc cst
    with e -> raise_as_env lc e

  let object_file_of_input file =
    let filename = Filename.chop_extension file in
    filename ^ ".dko"

  let export () =
    let file = object_file_of_input !current_file in
    let oc = open_out file in
    try Signature.export !sg oc; close_out oc
    with e -> raise_as_env dloc e

  let import lc md =
    try Signature.import !sg lc md
    with e -> raise_as_env lc e

  let _declare lc (id:ident) st ty : unit =
    match T.inference !sg ty with
    | Kind | Type _ -> Signature.add_declaration !sg lc id st ty
    | s -> raise (Typing.Typing_error (Typing.SortExpected (ty,[],s)))

  let is_static lc cst = Signature.is_static !sg lc cst

  (*         Rule checking       *)

  (* Checks that all Miller variables are applied to at least
     as many arguments on the rhs as they are on the lhs (their arity). *)
  let _check_arity (r:rule_infos) : unit =
    let check l id n k nargs =
      let expected_args = r.arity.(n-k) in
      if nargs < expected_args
      then raise_env l (NotEnoughArguments (id,n,nargs,expected_args)) in
    let rec aux k = function
      | Kind | Type _ | Const _ -> ()
      | DB (l,id,n) ->
        if n >= k then check l id n k 0
      | App(DB(l,id,n),a1,args) when n>=k ->
        check l id n k (List.length args + 1);
        List.iter (aux k) (a1::args)
      | App (f,a1,args) -> List.iter (aux k) (f::a1::args)
      | Lam (_,_,None,b) -> aux (k+1) b
      | Lam (_,_,Some a,b) | Pi (_,_,a,b) -> (aux k a;  aux (k+1) b)
    in
    aux 0 r.rhs

  (** Checks that all rule are left-linear. *)
  let _check_ll (r:rule_infos) : unit =
    List.iter
      (function Linearity _ -> raise (Entry.EnvError (Some (get_name()), r.l, NonLinearRule r.name)) | _ -> ())
      r.constraints

  let _add_rules rs =
    let ris = List.map Rule.to_rule_infos rs in
    if !check_arity then List.iter _check_arity ris;
    if !check_ll    then List.iter _check_ll    ris;
    Signature.add_rules !sg ris

  let _define lc (id:ident) (opaque:bool) (te:term) (ty_opt:Typing.typ option) : unit =
    let ty = match ty_opt with
      | None -> T.inference !sg te
      | Some ty -> T.checking !sg te ty; ty
    in
    match ty with
    | Kind -> raise_env lc (KindLevelDefinition id)
    | _ ->
      if opaque then Signature.add_declaration !sg lc id Signature.Static ty
      else
        let _ = Signature.add_declaration !sg lc id Signature.Definable ty in
        let cst = mk_name (get_name ()) id in
        let rule =
          { name= Delta(cst) ;
            ctx = [] ;
            pat = Pattern(lc, cst, []);
            rhs = te ;
          }
        in
        _add_rules [rule]

  let declare lc id st ty : unit =
    try _declare lc id st ty
    with e -> raise_as_env lc e

  let define lc id op te ty_opt : unit =
    try _define lc id op te ty_opt
    with e -> raise_as_env lc e

  let add_rules (rules: untyped_rule list) : (Subst.Subst.t * typed_rule) list =
    try
      let rs2 = List.map (T.check_rule !sg) rules in
      _add_rules rules;
      rs2
    with e -> raise_as_env (get_loc_rule (List.hd rules)) e

  let infer ?ctx:(ctx=[]) te =
    try
      let ty = T.infer !sg ctx te in
      (* We only verify that [ty] itself has a type (that we immediately
         throw away) if [ty] is not [Kind], because [Kind] does not have a
         type, but we still want [infer ctx Type] to produce [Kind] *)
      if ty <> mk_Kind then
        ignore(T.infer !sg ctx ty);
      ty
    with e -> raise_as_env (get_loc te) e

  let check ?ctx:(ctx=[]) te ty =
    try T.check !sg ctx te ty
    with e -> raise_as_env (get_loc te) e

  let _unsafe_reduction red te =
    R.reduction red !sg te

  let _reduction ctx red te =
    (* This is a safe reduction, so we check that [te] has a type
       before attempting to normalize it, but we only do so if [te]
       is not [Kind], because [Kind] does not have a type, but we
       still want to be able to reduce it *)
    if te <> mk_Kind then
      ignore(T.infer !sg ctx te);
    _unsafe_reduction red te

  let reduction ?ctx:(ctx=[]) ?red:(red=Reduction.default_cfg) te =
    try _reduction ctx red te
    with e -> raise_as_env (get_loc te) e

  let unsafe_reduction ?red:(red=Reduction.default_cfg) te =
    try _unsafe_reduction red te
    with e -> raise_as_env (get_loc te) e

  let are_convertible ?ctx:(ctx=[]) te1 te2 =
    try
      let ty1 = T.infer !sg ctx te1 in
      let ty2 = T.infer !sg ctx te2 in
      R.are_convertible !sg ty1 ty2 &&
      R.are_convertible !sg te1 te2
    with e -> raise_as_env (get_loc te1) e

end

module Default = Make(Reduction.Default)
