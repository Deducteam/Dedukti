open Basic
open Term
open Rule
open Typing
open Signature

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

type env_error =
  | EnvErrorType        of typing_error
  | EnvErrorSignature   of signature_error
  | EnvErrorRule        of rule_error
  | EnvErrorDep         of Dep.dep_error
  | NonLinearRule       of rule_name
  | NotEnoughArguments  of ident * int * int * int
  | KindLevelDefinition of ident
  | ParseError          of string
  | BracketScopingError
  | AssertError

exception EnvError of mident option * loc * env_error

let raise_as_env md lc = function
  | SignatureError e -> raise (EnvError (Some md, lc, (EnvErrorSignature e)))
  | TypingError    e -> raise (EnvError (Some md, lc, (EnvErrorType      e)))
  | RuleError      e -> raise (EnvError (Some md, lc, (EnvErrorRule      e)))
  | ex               -> raise ex

let check_arity = ref true

let check_ll = ref false

module HName = Hashtbl.Make(
  struct
    type t    = name
    let equal = name_eq
    let hash  = Hashtbl.hash
  end )

module type S =
sig
  type t
  module Printer : Pp.Printer with type t = t
  val raise_env : t -> loc -> env_error -> 'a

  val init        : string -> t

  val get_signature : t -> Signature.t
  val get_name    : t -> mident
  val get_symbols : t -> Signature.rw_infos HName.t
  val get_type    : t -> loc -> name -> term
  val is_static   : t -> loc -> name -> bool
  val get_dtree   : t -> loc -> name -> Dtree.t
  val export      : t -> unit
  val import      : t -> loc -> mident -> unit
  val declare     : t -> loc -> ident -> Signature.staticity -> term -> unit
  val define      : t -> loc -> ident -> bool -> term -> term option -> unit
  val add_rules   : t -> Rule.untyped_rule list -> (Subst.Subst.t * Rule.typed_rule) list

  val infer            : t -> ?ctx:typed_context -> term         -> term
  val check            : t -> ?ctx:typed_context -> term -> term -> unit
  val reduction        : t -> ?ctx:typed_context -> ?red:(Reduction.red_cfg) -> term -> term
  val are_convertible  : t -> ?ctx:typed_context -> term -> term -> bool
  val unsafe_reduction : t -> ?red:(Reduction.red_cfg) -> term -> term
end

(* Wrapper around Signature.t *)
module Make(R:Reduction.S) =
struct
  type t = Signature.t
  module T = Typing.Make(R)

  let init file = Signature.make file

  let get_name sg = Signature.get_name sg

  let get_signature sg = sg

  let raise_as_env sg x = raise_as_env (get_name sg) x
  let raise_env sg lc err = raise (EnvError (Some (get_name sg), lc, err))

  module Printer = Pp.Make(
    struct
      type t = Signature.t
      let get_name = get_name
    end)

  let get_symbols sg =
    let table = HName.create 11 in
    Signature.iter_symbols (fun md id -> HName.add table (mk_name md id)) sg;
    table

  let get_type sg lc cst =
    try Signature.get_type sg lc cst
    with e -> raise_as_env sg lc e

  let get_dtree sg lc cst =
    try Signature.get_dtree sg lc cst
    with e -> raise_as_env sg lc e

  let export sg =
    try Signature.export sg
    with e -> raise_as_env sg dloc e

  let import sg lc md =
    try Signature.import sg lc md
    with e -> raise_as_env sg lc e

  let _declare sg lc (id:ident) st ty : unit =
    match T.inference sg ty with
    | Kind | Type _ -> Signature.add_declaration sg lc id st ty
    | s -> raise (Typing.TypingError (Typing.SortExpected (ty,[],s)))

  let is_static sg lc cst = Signature.is_static sg lc cst

  (*         Rule checking       *)

  (* Checks that all Miller variables are applied to at least
     as many arguments on the rhs as they are on the lhs (their arity). *)
  let _check_arity sg (r:rule_infos) : unit =
    let check l id n k nargs =
      let expected_args = r.arity.(n-k) in
      if nargs < expected_args
      then raise_env sg l (NotEnoughArguments (id,n,nargs,expected_args)) in
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
  let _check_ll sg (r:rule_infos) : unit =
    List.iter
      (function Linearity _ -> raise (EnvError (Some (get_name sg), r.l, NonLinearRule r.name)) | _ -> ())
      r.constraints

  let _add_rules sg rs =
    let ris = List.map Rule.to_rule_infos rs in
    if !check_arity then List.iter (_check_arity sg) ris;
    if !check_ll    then List.iter (_check_ll    sg) ris;
    Signature.add_rules sg ris

  let _define sg lc (id:ident) (opaque:bool) (te:term) (ty_opt:Typing.typ option) : unit =
    let ty = match ty_opt with
      | None -> T.inference sg te
      | Some ty -> T.checking sg te ty; ty
    in
    match ty with
    | Kind -> raise_env sg lc (KindLevelDefinition id)
    | _ ->
      if opaque then Signature.add_declaration sg lc id Signature.Static ty
      else
        let _ = Signature.add_declaration sg lc id Signature.Definable ty in
        let cst = mk_name (get_name sg) id in
        let rule =
          { name= Delta(cst) ;
            ctx = [] ;
            pat = Pattern(lc, cst, []);
            rhs = te ;
          }
        in
        _add_rules sg [rule]

  let declare sg lc id st ty : unit =
    try _declare sg lc id st ty
    with e -> raise_as_env sg lc e

  let define sg lc id op te ty_opt : unit =
    try _define sg lc id op te ty_opt
    with e -> raise_as_env sg lc e

  let add_rules sg (rules: untyped_rule list) : (Subst.Subst.t * typed_rule) list =
    try
      let rs2 = List.map (T.check_rule sg) rules in
      _add_rules sg rules;
      rs2
    with e -> raise_as_env sg (get_loc_rule (List.hd rules)) e

  let infer sg ?ctx:(ctx=[]) te =
    try
      let ty = T.infer sg ctx te in
      (* We only verify that [ty] itself has a type (that we immediately
         throw away) if [ty] is not [Kind], because [Kind] does not have a
         type, but we still want [infer ctx Type] to produce [Kind] *)
      if ty <> mk_Kind then
        ignore(T.infer sg ctx ty);
      ty
    with e -> raise_as_env sg (get_loc te) e

  let check sg ?ctx:(ctx=[]) te ty =
    try T.check sg ctx te ty
    with e -> raise_as_env sg (get_loc te) e

  let _unsafe_reduction sg red te =
    R.reduction red sg te

  let _reduction sg ctx red te =
    (* This is a safe reduction, so we check that [te] has a type
       before attempting to normalize it, but we only do so if [te]
       is not [Kind], because [Kind] does not have a type, but we
       still want to be able to reduce it *)
    if te <> mk_Kind then
      ignore(T.infer sg ctx te);
    _unsafe_reduction sg red te

  let reduction sg ?ctx:(ctx=[]) ?red:(red=Reduction.default_cfg) te =
    try _reduction sg ctx red te
    with e -> raise_as_env sg (get_loc te) e

  let unsafe_reduction sg ?red:(red=Reduction.default_cfg) te =
    try _unsafe_reduction sg red te
    with e -> raise_as_env sg (get_loc te) e

  let are_convertible sg ?ctx:(ctx=[]) te1 te2 =
    try
      let ty1 = T.infer sg ctx te1 in
      let ty2 = T.infer sg ctx te2 in
      R.are_convertible sg ty1 ty2 &&
      R.are_convertible sg te1 te2
    with e -> raise_as_env sg (get_loc te1) e

end

module Default = Make(Reduction.Default)


module DynamicAPI :
sig
  include S
  val set_RE : (module Reduction.S) -> t -> t
  val get_RE : t -> (module Reduction.S)
end
=
struct
  type t =
    {
      sg : Signature.t;
      env: (module S with type t = Signature.t);
      red: (module Reduction.S)
    }
  let init md =
    {
      sg = Default.init md;
      red = (module Reduction.Default);
      env = (module Default)
    }
  let set_RE (module R:Reduction.S) sg =
    let red = (module R:Reduction.S) in
    let env = (module Make(R):S with type t = Signature.t) in
    { sg with red; env }
  let get_RE e = e.red

  let raise_env        s = let (module E) = s.env in E.raise_env s.sg
  let get_signature    s = let (module E) = s.env in E.get_signature s.sg
  let get_name         s = let (module E) = s.env in E.get_name s.sg
  let get_symbols      s = let (module E) = s.env in E.get_symbols s.sg
  let get_type         s = let (module E) = s.env in E.get_type s.sg
  let is_static        s = let (module E) = s.env in E.is_static s.sg
  let get_dtree        s = let (module E) = s.env in E.get_dtree s.sg
  let export           s = let (module E) = s.env in E.export s.sg
  let import           s = let (module E) = s.env in E.import s.sg
  let declare          s = let (module E) = s.env in E.declare s.sg
  let define           s = let (module E) = s.env in E.define s.sg
  let add_rules        s = let (module E) = s.env in E.add_rules s.sg
  let infer            s = let (module E) = s.env in E.infer s.sg
  let check            s = let (module E) = s.env in E.check s.sg
  let reduction        s = let (module E) = s.env in E.reduction s.sg
  let are_convertible  s = let (module E) = s.env in E.are_convertible s.sg
  let unsafe_reduction s = let (module E) = s.env in E.unsafe_reduction s.sg

  type aux = t
  module Printer = Pp.Make(
    struct
      type t = aux
      let get_name = get_name
    end)
end
