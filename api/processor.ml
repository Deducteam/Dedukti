open Kernel
open Basic
open Parsers

module type CustomEnv = (module type of Env) with type t = Env.t

module type S =
sig
  type t

  val handle_entry : Env.t -> Parsers.Entry.entry -> unit
  (** [handle_entry env entry] processed the entry [entry] in the environment [env] *)

  val get_data : Env.t -> t
  (** [get_data ()] returns the data computed by the current processor *)
end

module MakeTypeChecker(Env:CustomEnv) : S with type t = unit =
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
            pp_typed_rule r (Exsubst.ExSubst.pp (fun n -> let _,n,_ = List.nth r.ctx n in n)) s) rs
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

  let get_data _ = ()

end

module TypeChecker = MakeTypeChecker(Env)

module MakeSignatureBuilder(Env:CustomEnv) : S with type t = Signature.t =
struct
  type t = Signature.t

  let handle_entry env e =
    let sg = Env.get_signature env in
    let md = Env.get_name      env in
    let open Entry in
    match e with
    | Decl(lc,id,scope,st,ty) ->
      Signature.add_external_declaration sg lc (Basic.mk_name md id) scope st ty
    | Def(lc,id,scope,_,Some ty,te) ->
      let open Rule in
      Signature.add_external_declaration sg lc (Basic.mk_name md id) scope (Signature.Definable Term.Free) ty;
      let cst = Basic.mk_name md id in
      let rule = { name= Delta(cst) ; ctx = [] ; pat = Pattern(lc, cst, []); rhs = te ; } in
      Signature.add_rules sg [Rule.to_rule_infos rule]
    | Def(lc,_,_, _, None,_) ->
      raise @@ Typing.Typing_error (Typing.DomainFreeLambda lc) (* FIXME: It is not a typign error *)
    | Rules(_,rs) ->
      Signature.add_rules sg (List.map Rule.to_rule_infos rs)
    | Require(lc,md) -> Signature.import sg lc md
    | _ -> ()

  let get_data env =
   Env.get_signature env

end

module SignatureBuilder = MakeSignatureBuilder(Env)

module MakeEntryPrinter(Env:CustomEnv) : S with type t = unit =
struct

  type t = unit

  let handle_entry env e =
    let (module Pp:Pp.Printer) = (module Pp.Make(struct let get_name () = Env.get_name env end)) in
    Pp.print_entry Format.std_formatter e

  let get_data _ = ()

end

module EntryPrinter = MakeEntryPrinter(Env)

module MakeDependencies(Env:CustomEnv) : S with type t = Dep.t =
struct
  type t = Dep.t

  let handle_entry env e = Dep.handle (Env.get_name env) (fun f -> f e)

  let get_data _ = Dep.deps
end

module Dependencies = MakeDependencies(Env)

module MakeTopLevel(Env:CustomEnv) : S with type t = unit =
struct

  type t = unit

  module Printer = Pp.Default

  let print fmt =
    Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt


  let handle_entry env e =
    let open Entry in
    match e with
    | Decl(lc,id,scope,st,ty) ->
      Env.declare env lc id scope st ty;
      Format.printf "%a is declared.@." pp_ident id
    | Def(lc,id,scope,op,ty,te) ->
      Env.define env lc id scope op te ty;
      Format.printf "%a is defined.@." pp_ident id
    | Rules(_,rs) ->
      let _ = Env.add_rules env rs in
      List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) rs
    | Eval(_,red,te) ->
      let te = Env.reduction env ~red te in
      Format.printf "%a@." Printer.print_term te
    | Infer(_,red,te) ->
      let  ty = Env.infer env te in
      let rty = Env.reduction env ~red ty in
      Format.printf "%a@." Printer.print_term rty
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
      Format.printf "GDTs for symbol %a:\n%a" pp_name cst Dtree.pp_dforest forest
    | Print(_,s) -> Format.printf "%s@." s
    | Name _     -> Format.printf "\"#NAME\" directive ignored.@."
    | Require _  -> Format.printf "\"#REQUIRE\" directive ignored.@."

  let get_data _ = ()
end

module TopLevel = MakeTopLevel(Env)

type _ t = ..

module Registration =
struct
  type pack_processor = Pack : 'a t *  (module S with type t = 'a) -> pack_processor

  let dispatch : pack_processor list ref = ref []

  exception Not_registered_processor

  type (_,_) equal =
    | Refl : 'a -> ('a,'a) equal

  type equality =
    {
      equal: 'a 'b. ('a t * 'b t) -> ('a t,'b t) equal option
    }

  let equal : equality ref = ref {equal = fun _ -> None}

  let register_processor  (type a) : a t -> equality -> (module S with type t = a) -> unit =
    fun (processor : a t) f (module P : S with type t = a) ->
    dispatch := Pack (processor, (module P : S with type t = a)) :: !dispatch;
    let old_equal = !equal in
    equal := {equal = fun pair ->
        match f.equal pair with
        | Some refl -> Some refl
        | None -> old_equal.equal pair}

end

let get_processor (type a) : a t -> (module S with type t = a)  = fun processor ->
  let open Registration in
  let dispatch = !dispatch in
  let rec unpack' list =
    match list with
    | [] -> raise Not_registered_processor
    | Pack (processor', (module P))::list' ->
      match !equal.equal (processor, processor') with
      | Some (Refl _) ->  (module P : ( S with type t = a))
        | None -> unpack' list'
  in
  unpack' dispatch


type processor_error = Env.t * Kernel.Basic.loc * exn

type hook =
  {
    before : Env.t -> unit ;
    after  : Env.t -> processor_error option -> unit
  }

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

let handle_input  : Parser.input -> ?hook:hook -> 'a t -> 'a =
  fun (type a) input ?hook processor ->
  let (module P: S with type t = a) = get_processor processor in
  let env = Env.init input in
  begin match hook with None -> () | Some hook -> hook.before env end;
  let exn =
    try
      handle_processor env (module P);
      None
    with Env.Env_error(env,lc,e) -> Some (env,lc,e)
  in
  begin
    match hook with
    | None ->
      begin
        match exn with
        | None -> ()
        | Some(env,lc,exn) -> Env.fail_env_error env lc exn
      end
    | Some hook -> hook.after env exn end;
  let data = P.get_data env in
  data

let fold_files : string list -> ?hook:hook -> f:('a -> 'b -> 'b) -> default:'b -> 'a t -> 'b =
  fun files ?hook ~f ~default processor ->
  let handle_file file =
    try
      let input = Parser.input_from_file file in
      let data = handle_input input ?hook processor in
      Parser.close input;
      data
    with Sys_error msg -> Errors.fail_sys_error ~file ~msg
  in
  let fold b file =
    try f (handle_file file) b
    with exn ->
      let env = Env.init (Parser.input_from_file file) in
      Env.fail_env_error env Basic.dloc exn
  in
  List.fold_left fold default files

let handle_files : string list -> ?hook:hook -> 'a t -> 'a =
  fun (type a) files ?hook processor ->
  let (module P: S with type t = a) = get_processor processor in
  fold_files files ?hook ~f:(fun data _ -> data) ~default:(P.get_data (Env.dummy ())) processor

let of_pure (type a) ~f ~init : (module S with type t = a) =
  (module struct
    type t = a

    let _d = ref init

    let handle_entry env entry = _d := f !_d env entry

    let get_data _ = !_d
  end)





type _ t += TypeChecker : unit t

type _ t += SignatureBuilder : Signature.t t

type _ t += PrettyPrinter : unit t

type _ t += Dependencies : Dep.t t

type _ t += TopLevel : unit t

let equal_tc (type a b) : (a t * b t) -> (a t,b t) Registration.equal option =
  function
  | TypeChecker, TypeChecker -> Some (Registration.Refl (TypeChecker))
  | _ -> None

let equal_sb (type a b) : (a t * b t) -> (a t,b t) Registration.equal option =
  function
  | SignatureBuilder, SignatureBuilder -> Some (Refl (SignatureBuilder))
  | _ -> None

let equal_pp (type a b) : (a t * b t) -> (a t,b t) Registration.equal option =
  function
  | PrettyPrinter, PrettyPrinter -> Some (Refl (PrettyPrinter))
  | _ -> None

let equal_dep (type a b) : (a t * b t) -> (a t,b t) Registration.equal option =
  function
  | Dependencies, Dependencies -> Some (Refl (Dependencies))
  | _ -> None

let equal_top_level (type a b) : (a t * b t) -> (a t,b t) Registration.equal option =
  function
  | TopLevel, TopLevel -> Some (Refl (TopLevel))
  | _ -> None

let () =
  let open Registration in
  register_processor TypeChecker {equal = equal_tc}
    (module TypeChecker);
  register_processor SignatureBuilder {equal = equal_sb}
    (module SignatureBuilder);
  register_processor PrettyPrinter {equal = equal_pp}
    (module EntryPrinter);
  register_processor Dependencies {equal = equal_dep}
    (module Dependencies);
  register_processor TopLevel {equal = equal_top_level}
    (module TopLevel)
