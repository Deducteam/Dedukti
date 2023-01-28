open Kernel
open Basic
open Parsers

module type S = sig
  type t = Env.t

  type output

  val handle_entry : t -> Parsers.Entry.entry -> t

  val output : t -> output
end

module TypeChecker : S with type output = unit = struct
  type t = Env.t

  type output = unit

  let handle_entry env e =
    let open Entry in
    let (module Pp : Pp.Printer) = Env.get_printer env in
    match e with
    | Decl (lc, id, scope, st, ty) ->
        Debug.(debug d_notice) "Declaration of constant '%a'." pp_ident id;
        Env.declare env lc id scope st ty
    | Def (lc, id, scope, opaque, ty, te) ->
        let opaque_str = if opaque then " (opaque)" else "" in
        Debug.(debug d_notice)
          "Definition of symbol '%a'%s." pp_ident id opaque_str;
        Env.define env lc id scope opaque te ty
    | Rules (_, rs) ->
        let open Rule in
        List.iter
          (fun (r : partially_typed_rule) ->
            Debug.(
              debug d_notice "Adding rewrite rules: '%a'" Pp.print_rule_name
                r.name))
          rs;
        let rs = Env.add_rules env rs in
        List.iter
          (fun (s, r) ->
            Debug.debug Debug.d_notice "%a@.with the following constraints: %a"
              pp_typed_rule r
              (Exsubst.ExSubst.pp (fun n ->
                   let _, n, _ = List.nth r.ctx n in
                   n))
              s)
          rs
    | Eval (_, red, te) ->
        let te = Env.reduction env ~red te in
        Format.printf "%a@." Pp.print_term te
    | Infer (_, red, te) ->
        let ty = Env.infer env te in
        let rty = Env.reduction env ~red ty in
        Format.printf "%a@." Pp.print_term rty
    | Check (lc, assrt, neg, Convert (t1, t2)) -> (
        let succ = Env.are_convertible env t1 t2 <> neg in
        match (succ, assrt) with
        | true, false -> Format.printf "YES@."
        | true, true -> ()
        | false, false -> Format.printf "NO@."
        | false, true -> raise @@ Entry.Assert_error lc)
    | Check (lc, assrt, neg, HasType (te, ty)) -> (
        let succ = try Env.check env te ty; not neg with _ -> neg in
        match (succ, assrt) with
        | true, false -> Format.printf "YES@."
        | true, true -> ()
        | false, false -> Format.printf "NO@."
        | false, true -> raise @@ Entry.Assert_error lc)
    | DTree (lc, m, v) ->
        let m = match m with None -> Env.get_name env | Some m -> m in
        let cst = mk_name m v in
        let forest = Env.get_dtree env lc cst in
        Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest
          forest
    | Print (_, s) -> Format.printf "%s@." s
    | Name (_, n) ->
        if not (mident_eq n (Env.get_name env)) then
          Debug.(debug d_warn "Invalid #NAME directive ignored.@.")
    | Require (lc, md) -> Env.import env lc md
    | Pragma (lc, str) ->
        Format.eprintf "Unsupported pragma at position %a: '%s'@." pp_loc lc str

  let handle_entry env entry = handle_entry env entry; env

  let output _env = ()
end

module SignatureBuilder : S with type output = Signature.t = struct
  type t = Env.t

  type output = Signature.t

  let handle_entry env e =
    let sg = Env.get_signature env in
    let md = Env.get_name env in
    let open Entry in
    match e with
    | Decl (lc, id, scope, st, ty) ->
        Signature.add_external_declaration sg lc (Basic.mk_name md id) scope st
          ty
    | Def (lc, id, scope, _, Some ty, te) ->
        let open Rule in
        Signature.add_external_declaration sg lc (Basic.mk_name md id) scope
          (Signature.Definable Term.Free) ty;
        let cst = Basic.mk_name md id in
        let rule =
          {name = Delta cst; ctx = []; pat = Pattern (lc, cst, []); rhs = te}
        in
        Signature.add_rules sg [Rule.to_rule_infos rule]
    | Def (lc, _, _, _, None, _) ->
        raise @@ Typing.Typing_error (Typing.DomainFreeLambda lc)
        (* FIXME: It is not a typign error *)
    | Rules (_, rs) -> Signature.add_rules sg (List.map Rule.to_rule_infos rs)
    | Require (lc, md) -> Signature.import sg lc md
    | _ -> ()

  let handle_entry env entry = handle_entry env entry; env

  let output env = Env.get_signature env
end

module EntryPrinter : S with type output = unit = struct
  type t = Env.t

  type output = unit

  let handle_entry env e =
    let (module Pp : Pp.Printer) =
      (module Pp.Make (struct
        let get_name () = Env.get_name env
      end))
    in
    Pp.print_entry Format.std_formatter e

  let handle_entry env entry = handle_entry env entry; env

  let output _env = ()
end

module Dependencies : S with type output = Dep_legacy.t = struct
  type t = Env.t

  type output = Dep_legacy.t

  let handle_entry env e = Dep_legacy.handle (Env.get_name env) (fun f -> f e)

  let handle_entry env entry = handle_entry env entry; env

  let output _ = Dep_legacy.deps
end

module TopLevel : S with type output = unit = struct
  type output = unit

  type t = Env.t

  module Printer = Pp.Default

  let print fmt =
    Format.kfprintf (fun _ -> print_newline ()) Format.std_formatter fmt

  let handle_entry env e =
    let open Entry in
    match e with
    | Decl (lc, id, scope, st, ty) ->
        Env.declare env lc id scope st ty;
        Format.printf "%a is declared.@." pp_ident id
    | Def (lc, id, scope, op, ty, te) ->
        Env.define env lc id scope op te ty;
        Format.printf "%a is defined.@." pp_ident id
    | Rules (_, rs) ->
        let _ = Env.add_rules env rs in
        List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) rs
    | Eval (_, red, te) ->
        let te = Env.reduction env ~red te in
        Format.printf "%a@." Printer.print_term te
    | Infer (_, red, te) ->
        let ty = Env.infer env te in
        let rty = Env.reduction env ~red ty in
        Format.printf "%a@." Printer.print_term rty
    | Check (lc, assrt, neg, Convert (t1, t2)) -> (
        let succ = Env.are_convertible env t1 t2 <> neg in
        match (succ, assrt) with
        | true, false -> Format.printf "YES@."
        | true, true -> ()
        | false, false -> Format.printf "NO@."
        | false, true -> raise @@ Entry.Assert_error lc)
    | Check (lc, assrt, neg, HasType (te, ty)) -> (
        let succ = try Env.check env te ty; not neg with _ -> neg in
        match (succ, assrt) with
        | true, false -> Format.printf "YES@."
        | true, true -> ()
        | false, false -> Format.printf "NO@."
        | false, true -> raise @@ Entry.Assert_error lc)
    | DTree (lc, m, v) ->
        let m = match m with None -> Env.get_name env | Some m -> m in
        let cst = mk_name m v in
        let forest = Env.get_dtree env lc cst in
        Format.printf "GDTs for symbol %a:\n%a" pp_name cst Dtree.pp_dforest
          forest
    | Print (_, s) -> Format.printf "%s@." s
    | Name _ -> Format.printf "\"#NAME\" directive ignored.@."
    | Require _ -> Format.printf "\"#REQUIRE\" directive ignored.@."
    | Pragma _ -> Format.printf "Pragma directive ignored.@."

  let handle_entry env entry = handle_entry env entry; env

  let output _ = ()
end

type _ t = ..

module Registration = struct
  type pack_processor =
    | Pack : 'a t * (module S with type output = 'a) -> pack_processor

  let dispatch : pack_processor list ref = ref []

  exception Not_registered_processor

  type (_, _) equal = Refl : 'a -> ('a, 'a) equal

  type equality = {equal : 'a 'b. 'a t * 'b t -> ('a t, 'b t) equal option}

  let equal : equality ref = ref {equal = (fun _ -> None)}

  let register_processor (type a) :
      a t -> equality -> (module S with type output = a) -> unit =
   fun (processor : a t) f (module P : S with type output = a) ->
    dispatch :=
      Pack (processor, (module P : S with type output = a)) :: !dispatch;
    let old_equal = !equal in
    equal :=
      {
        equal =
          (fun pair ->
            match f.equal pair with
            | Some refl -> Some refl
            | None -> old_equal.equal pair);
      }
end

let get_processor (type a) : a t -> (module S with type output = a) =
 fun processor ->
  let open Registration in
  let dispatch = !dispatch in
  let rec unpack' list : (module S with type output = a) =
    match list with
    | [] -> raise Not_registered_processor
    | Pack (processor', (module P : S with type output = _)) :: list' -> (
        match !equal.equal (processor, processor') with
        | Some (Refl _) ->
            (module struct
              include P
            end : S
              with type output = _)
        | None -> unpack' list')
  in
  unpack' dispatch

type processor_error = Env.t * Kernel.Basic.loc * exn

type hook = {
  before : Parsers.Parser.input -> Env.t -> unit;
  after : Parsers.Parser.input -> Env.t -> processor_error option -> unit;
}

module type Interface = sig
  type 'a t

  val handle_input :
    ?hook:hook -> Files.load_path -> input:Parsers.Parser.input -> 'a t -> 'a

  val handle_files :
    ?hook:hook -> Files.load_path -> files:string list -> 'a t -> 'a

  val fold_files :
    ?hook:hook ->
    Files.load_path ->
    files:string list ->
    f:('a -> 'b -> 'b) ->
    default:'b ->
    'a t ->
    'b
end

module Make (C : sig
  type 'a t

  val get_processor : 'a t -> (module S with type output = 'a)
end) : Interface with type 'a t := 'a C.t = struct
  type 'a t = 'a C.t

  let handle_processor : Parsers.Parser.input -> Env.t -> (module S) -> unit =
   fun input env (module P : S) ->
    try
      let handle_entry env entry =
        try P.handle_entry env entry
        with exn -> raise @@ Env.Env_error (env, Entry.loc_of_entry entry, exn)
      in
      ignore @@ (Parser.to_seq_exn input |> Seq.fold_left handle_entry env)
    with
    | Env.Env_error _ as exn -> raise @@ exn
    | exn -> raise @@ Env.Env_error (env, Basic.dloc, exn)

  let handle_input :
      ?hook:hook -> Files.load_path -> input:Parser.input -> 'a t -> 'a =
    fun (type a) ?hook load_path ~input processor ->
     let (module P : S with type output = a) = C.get_processor processor in
     let md = Parsers.Parser.md_of_input input in
     let env = Env.init load_path md in
     (match hook with None -> () | Some hook -> hook.before input env);
     let exn =
       try
         handle_processor input env (module P);
         None
       with Env.Env_error (env, lc, e) -> Some (env, lc, e)
     in
     (match hook with
     | None -> (
         match exn with
         | None -> ()
         | Some (_env, loc, exn) -> Errors.fail_exn input loc exn)
     | Some hook -> hook.after input env exn);
     let data = P.output env in
     data

  let fold_files :
      ?hook:hook ->
      Files.load_path ->
      files:string list ->
      f:('a -> 'b -> 'b) ->
      default:'b ->
      'a t ->
      'b =
   fun ?hook load_path ~files ~f ~default processor ->
    let handle_input input =
      try
        let data = handle_input ?hook load_path ~input processor in
        Parser.close input; data
      with Sys_error msg -> Errors.fail_sys_error input ~msg
    in
    let fold b file =
      let input = Parser.from_file ~file in
      try f (handle_input input) b
      with exn -> Errors.fail_exn input Basic.dloc exn
    in
    List.fold_left fold default files

  let handle_files :
      ?hook:hook -> Files.load_path -> files:string list -> 'a t -> 'a =
    fun (type a) ?hook load_path ~files processor ->
     let (module P : S with type output = a) = C.get_processor processor in
     fold_files load_path ~files ?hook
       ~f:(fun data _ -> data)
       ~default:(P.output (Env.dummy ()))
       processor
end

let of_pure (type a) ~f ~init : (module S with type output = a) =
  (module struct
    type t = Env.t

    type output = a

    let _d = ref init

    let handle_entry env entry =
      _d := f !_d env entry;
      env

    let output _ = !_d
  end)

type _ t += TypeChecker : unit t

type _ t += SignatureBuilder : Signature.t t

type _ t += PrettyPrinter : unit t

type _ t += Dependencies : Dep_legacy.t t

type _ t += TopLevel : unit t

let equal_tc (type a b) : a t * b t -> (a t, b t) Registration.equal option =
  function
  | TypeChecker, TypeChecker -> Some (Registration.Refl TypeChecker)
  | _ -> None

let equal_sb (type a b) : a t * b t -> (a t, b t) Registration.equal option =
  function
  | SignatureBuilder, SignatureBuilder -> Some (Refl SignatureBuilder)
  | _ -> None

let equal_pp (type a b) : a t * b t -> (a t, b t) Registration.equal option =
  function
  | PrettyPrinter, PrettyPrinter -> Some (Refl PrettyPrinter)
  | _ -> None

let equal_dep (type a b) : a t * b t -> (a t, b t) Registration.equal option =
  function
  | Dependencies, Dependencies -> Some (Refl Dependencies)
  | _ -> None

let equal_top_level (type a b) :
    a t * b t -> (a t, b t) Registration.equal option = function
  | TopLevel, TopLevel -> Some (Refl TopLevel)
  | _ -> None

let () =
  let open Registration in
  register_processor TypeChecker {equal = equal_tc} (module TypeChecker);
  register_processor SignatureBuilder {equal = equal_sb}
    (module SignatureBuilder);
  register_processor PrettyPrinter {equal = equal_pp} (module EntryPrinter);
  register_processor Dependencies {equal = equal_dep} (module Dependencies);
  register_processor TopLevel {equal = equal_top_level} (module TopLevel)

include Make (struct
  type nonrec 'a t = 'a t

  let get_processor = get_processor
end)

module T = Make (struct
  type 'a t = (module S with type output = 'a)

  let get_processor x = x
end)
