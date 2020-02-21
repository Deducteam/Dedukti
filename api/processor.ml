open Kernel
open Basic
open Parsers

module type ENV = (module type of Env with type t = Env.t)

module type S =
sig
  type t

  val handle_entry : Env.t -> Entry.entry -> unit

  val get_data : unit -> t
  val hook_before  : Env.t -> unit
  val hook_error   : Env.t -> (Env.t * Kernel.Basic.loc * exn) -> unit
  val hook_success : Env.t -> t -> unit
end

module MakeTypeChecker(Env:ENV) : S with type t = unit =
struct

  type t = unit

  let handle_entry env e =
    let open Entry in
    let (module Pp:Pp.Printer) = Env.get_printer env in
    match e with
    | Decl(lc,id,scope,st,ty) ->
      Debug.(debug d_notice) "Declaration of constant '%a'." pp_ident id;
      Env.declare env lc id scope st ty
    | Def(lc,id,scope,opaque,ty,te) ->
      let opaque_str = if opaque then " (opaque)" else "" in
      Debug.(debug d_notice) "Definition of symbol '%a'%s." pp_ident id opaque_str;
      Env.define env lc id scope opaque te ty
    | Rules(_,rs) ->
      let open Rule in
      List.iter (fun (r:partially_typed_rule) ->
          Debug.(debug d_notice "Adding rewrite rules: '%a'" Pp.print_rule_name r.name)) rs;
      let rs = Env.add_rules env rs in
      List.iter (fun (s,r) ->
          Debug.debug Debug.d_notice "%a@.with the following constraints: %a"
            pp_typed_rule r (Subst.Subst.pp (fun n -> let _,n,_ = List.nth r.ctx n in n)) s) rs
    | Eval(_,red,te) ->
      let te = Env.reduction env ~red te in
      Format.printf "%a@." Pp.print_term te
    | Infer(_,red,te) ->
      let  ty = Env.infer env te in
      let rty = Env.reduction env ~red ty in
      Format.printf "%a@." Pp.print_term rty
    | Check(lc, assrt, neg, Convert(t1,t2)) ->
      let succ = (Env.are_convertible env t1 t2) <> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> raise @@ Entry.Assert_error lc)
    | Check(lc, assrt, neg, HasType(te,ty)) ->
      let succ = try Env.check env te ty; not neg with _ -> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> raise @@ Entry.Assert_error lc)
    | DTree(lc,m,v) ->
      let m = match m with None -> Env.get_name env | Some m -> m in
      let cst = mk_name m v in
      let forest = Env.get_dtree env lc cst in
      Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest forest
    | Print(_,s) -> Format.printf "%s@." s
    | Name(_,n) ->
      if not (mident_eq n (Env.get_name env))
      then Debug.(debug d_warn "Invalid #NAME directive ignored.@.")
    | Require(lc,md) -> Env.import env lc md

  let get_data () = ()

  let hook_before _ = ()
  let hook_error _ (env, lc, e) = Env.fail_env_error env lc e
  let hook_success _ _ = ()
end

module TypeChecker = MakeTypeChecker(Env)

module MakeSignatureBuilder(Env:ENV) : S with type t = Signature.t =
struct
  type t = Signature.t

  let sg : Signature.t option ref = ref None

  let handle_entry env e =
    sg := Some (Env.get_signature env);
    let sg = Env.get_signature env in
    let md = Env.get_name      env in
    let open Entry in
    match e with
    | Decl(lc,id,scope,st,ty) ->
      Signature.add_external_declaration sg lc (Basic.mk_name md id) scope st ty
    | Def(lc,id,scope,_,Some ty,te) ->
      let open Rule in
      Signature.add_external_declaration sg lc (Basic.mk_name md id) scope Signature.Definable ty;
      let cst = Basic.mk_name md id in
      let rule = { name= Delta(cst) ; ctx = [] ; pat = Pattern(lc, cst, []); rhs = te ; } in
      Signature.add_rules sg [Rule.to_rule_infos rule]
    | Def(lc,_,_, _, None,_) ->
      raise @@ Typing.Typing_error (Typing.DomainFreeLambda lc)
    (* FIXME: It is not a typign error *)
    | Rules(_,rs) ->
      Signature.add_rules sg (List.map Rule.to_rule_infos rs)
    | Require(lc,md) -> Signature.import sg lc md
    | _ -> ()

  let get_data () =
    match !sg with
    | None -> Signature.make (mk_mident "") Files.find_object_file
    (*TODO: raise an error? *)
    | Some sg -> sg

  let hook_before _ = ()
  let hook_error _ (env, lc, e) = Env.fail_env_error env lc e
  let hook_success _ _ = ()
end

module SignatureBuilder = MakeSignatureBuilder(Env)

module MakeEntryPrinter(Env:ENV) : S with type t = unit =
struct

  type t = unit

  let handle_entry env e =
    let (module Pp:Pp.Printer) = (module Pp.Make(struct let get_name () = Env.get_name env end)) in
    Pp.print_entry Format.std_formatter e

  let get_data () = ()

  let hook_before _ = ()
  let hook_error _ (env, lc, e) = Env.fail_env_error env lc e
  let hook_success _ _ = ()
end

module EntryPrinter = MakeEntryPrinter(Env)

module MakeDependencies(Env:ENV) : S with type t = Dep.t =
struct
  type t = Dep.t

  let handle_entry env e = Dep.handle (Env.get_name env) (fun f -> f e)

  let get_data () = Dep.deps

  let hook_before _ = ()
  let hook_error _ (env, lc, e) = Env.fail_env_error env lc e
  let hook_success _ _ = ()
end

module Dependencies = MakeDependencies(Env)

let handle_processor : Env.t -> (module S) -> unit  =
  fun env (module P:S) ->
  let input = Env.get_input env in
  try
    let handle_entry env entry =
      try
        P.handle_entry env entry
      with exn -> raise @@ Env.Env_error(env, Entry.loc_of_entry entry, exn)
    in
    Parser.handle input (handle_entry env)
  with
  | Env.Env_error _ as exn -> raise @@ exn
  |  exn                   -> raise @@ Env.Env_error(env, Basic.dloc, exn)

let handle_input  : type a. Parser.t ->
  (module S with type t = a) -> a =
  fun (type a) input (module P:S with type t = a) ->
  let env = Env.init input in
  P.hook_before env;
  let exn =
    try
      handle_processor env (module P);
      None
    with Env.Env_error(env,lc,e) -> Some (env,lc,e)
  in
  begin
    match exn with
    | None -> P.hook_success env (P.get_data ())
    | Some e -> P.hook_error env e
  end;
  let data = P.get_data () in
  data

let handle_files : string list ->
  (module S with type t = 'a) -> 'a =
  fun (type a) files (module P:S with type t = a) ->
  let handle_file file =
    try
      let input = Parser.input_from_file file in
      ignore(handle_input input (module P));
      Parser.close input
    with Sys_error msg -> Errors.fail_sys_error ~file ~msg
  in
  List.iter (handle_file) files;
  P.get_data ()

let process_files : string list ->
  ('a -> 'b -> 'b) -> 'b ->
  (module S with type t = 'a) -> 'b =
  fun (type a b) files (fold:a -> b -> b) (neutral:b) (module P:S with type t = a) ->
  let handle_file file =
    try
      let input = Parser.input_from_file file in
      let data = handle_input input (module P) in
      Parser.close input;
      data
    with Sys_error msg -> Errors.fail_sys_error ~file ~msg
  in
  let fold b file =
    try fold (handle_file file) b
    with exn ->
      let env = Env.init (Parser.input_from_file file) in
      Env.fail_env_error env Basic.dloc exn
  in
  List.fold_left fold neutral files

let of_pure (type a) ~f ~init : (module S with type t = a) =
  (module struct
    type t = a

    let _d = ref init

    let handle_entry env entry = _d := f !_d env entry

    let get_data () = !_d

    let hook_before _ = ()
    let hook_error _ (env, lc, e) = Env.fail_env_error env lc e
    let hook_success _ _ = ()
  end)

module MakeEnv (R:Reduction.S) = struct
  include Env
  let init env =
    let env = init env in
    set_reduction_engine env (module R)
end
