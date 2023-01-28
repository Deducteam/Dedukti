(** Main file *)
module B = Kernel.Basic

module C = Common.Constraints
module E = Elaboration.Elaborate
module F = Common.Files
module L = Common.Log
module M = Api.Meta
module P = Parsers.Parser
module S = Kernel.Signature
module O = Common.Oracle
module U = Common.Universes

(** Direct the control flow of Universo. The control flow of Universo can be sum up in 4 steps:
    1) Elaborate the files to replace universes by variables
    2) Check the files to generate constraints
    3) Solve the constraints
    4) Reconstruct the files with the solution *)
type execution_mode =
  | Normal  (** Go through the four steps *)
  | JustElaborate  (** Do not generate constraints (only step 1). *)
  | JustCheck
      (** Only generate constraints (only step 2). ASSUME that elaboration has been done before. *)
  | JustSolve
      (** Only solve the constraints (only step 3). ASSUME that constraints has been generated before. *)
  | Simplify

(** By default, Universo go through all the steps *)
let mode = ref Normal

module Cmd = struct
  module B = Kernel.Basic
  module E = Parsers.Entry
  module F = Common.Files
  module L = Common.Logic
  module M = Api.Meta
  module O = Common.Oracle
  module P = Parsers.Parser
  module R = Kernel.Rule
  module S = Kernel.Signature
  module T = Kernel.Term
  module U = Common.Universes

  (** The path that contains the configuration file *)
  let config_path = ref "universo_cfg.dk"

  let config = Hashtbl.create 11

  type cmd_error =
    | ConfigurationFileNotFound of string
    | NoTargetSpecification
    | WrongConfiguration of E.entry
    | NoOutputSection
    | NoElaborationSection
    | Misc of string

  exception Cmd_error of cmd_error

  let sections =
    [
      "elaboration";
      "output";
      "constraints";
      "solver";
      "lra_specification";
      "qfuf_specification";
      "end";
    ]

  let parse_config : unit -> unit =
   fun () ->
    let module P = struct
      type t = unit

      let section = ref ""

      let parameters = ref []

      let handle_entry env =
        let (module Printer) = Api.Env.get_printer env in
        let check_section s = List.mem s sections in
        function
        | E.Decl (_, id, _, _, _) when check_section (B.string_of_ident id) ->
            if !section <> "" then (
              Hashtbl.add config !section (List.rev !parameters);
              parameters := []);
            section := B.string_of_ident id
        | E.Rules (_, rs) -> parameters := rs @ !parameters
        | _ as e ->
            raise
            @@ Cmd_error
                 (Misc
                    (Format.asprintf
                       "Configuration file (entry not recognized): %a"
                       Printer.print_entry e))

      let get_data _ = ()
    end in
    (* Load path is not needed since no importation is done by the
       [P] processor. *)
    let load_path = Api.Files.empty in
    Api.Processor.T.handle_files ~load_path ~files:[!config_path] (module P)

  let elaboration_meta_cfg : load_path:Api.Files.t -> M.cfg =
   fun ~load_path ->
    let meta_rules =
      try Hashtbl.find config "elaboration"
      with _ -> raise @@ Cmd_error NoElaborationSection
    in
    M.default_config ~meta_rules ~load_path ()

  let output_meta_cfg : load_path:Api.Files.t -> M.cfg =
   fun ~load_path ->
    let meta_rules =
      try Hashtbl.find config "output"
      with _ -> raise @@ Cmd_error NoOutputSection
    in
    M.default_config ~meta_rules ~load_path ()

  let mk_constraints : unit -> (B.name, U.pred) Hashtbl.t =
   fun () ->
    let table = Hashtbl.create 11 in
    let mk_rule : R.partially_typed_rule -> unit =
     fun r ->
      let name =
        match r.pat with
        | R.Pattern (_, name, []) -> name
        | _ -> failwith "Constraints are not in correct format"
      in
      (* let pred = try U.extract_pred (M.mk_term meta r.rhs) *)
      let pred =
        try U.extract_pred r.rhs
        with U.Not_pred -> failwith "Constraints are not in correct format"
      in
      Hashtbl.add table name pred
    in
    (try List.iter mk_rule (Hashtbl.find config "constraints") with _ -> ());
    table

  (** [add_rules sg rs] add the rewrite rules [rs] to the signature [sg] *)
  let add_rules sg rs =
    (* Several rules might be bound to different constant *)
    let add_rule sg r = S.add_rules sg [R.to_rule_infos r] in
    List.iter (add_rule sg) rs

  (** [to_elaboration_env f] generates a fresh environement to elaborate file [f]. *)
  let to_elaboration_env :
      load_path:Api.Files.t -> F.path -> Elaboration.Elaborate.t =
   fun ~load_path in_path ->
    let file = F.out_from_string in_path `Elaboration in
    {file; meta = elaboration_meta_cfg ~load_path}

  (** [mk_theory ()] returns the theory used by universo. *)
  let mk_theory : load_path:Api.Files.t -> S.t =
   fun ~load_path ->
    Api.Processor.(
      handle_files ~load_path
        ~files:[F.get_theory ()]
        Api.Processor.SignatureBuilder)

  (* (\** [elab_signature f] returns the signature containing all the universes declaration associated to *)
  (*     file [f] *\) *)
  (* let elab_signature : string -> S.t = fun in_path -> *)
  (*   F.signature_of_file (F.get_out_path in_path `Elaboration) *)

  (** [to_checking_env f] returns the type checking environement for the file [f] *)
  let to_checking_env : load_path:Api.Files.t -> string -> Checking.Checker.t =
   fun ~load_path in_path ->
    (* FIXME: UGLY, rework to match the new API *)
    let out = F.get_out_path in_path `Output in
    let env = Api.Env.init ~load_path ~input:(P.from_file ~file:out) in
    let constraints = mk_constraints () in
    let out_file = F.out_from_string in_path `Checking in
    {env; in_path; meta_out = output_meta_cfg ~load_path; constraints; out_file}

  (** [theory_meta f] returns the meta configuration that allows to elaborate a theory for the SMT solver *)
  let mk_smt_theory : load_path:Api.Files.t -> int -> O.theory =
   fun ~load_path ->
    try
      let meta_rules = Hashtbl.find config "qfuf_specification" in
      let meta = output_meta_cfg ~load_path in
      M.add_rules meta meta_rules;
      O.mk_theory meta
    with Not_found -> raise @@ Cmd_error NoTargetSpecification

  let find_predicate s r =
    match r.R.pat with
    | Pattern (_, n', _) -> B.string_of_ident (B.id n') = s
    | _ -> false

  let get_lra_specification_config : string -> string list * T.term =
   fun s ->
    try
      let rs = Hashtbl.find config "lra_specification" in
      let r = List.find (find_predicate s) rs in
      let to_string = function
        | R.Var (_, id, _, _) -> B.string_of_ident id
        | _ -> assert false
      in
      match r.pat with
      | R.Pattern (_, _, l) -> (List.map to_string l, r.rhs)
      | _ -> assert false
    with _ -> raise @@ Cmd_error (Misc "Wrong solver specification")

  let mk_lra_reification : unit -> (module L.LRA_REIFICATION) =
   fun () ->
    (module struct
      let axiom_specification = get_lra_specification_config "axiom"

      let rule_specification = get_lra_specification_config "rule"

      let cumul_specification = get_lra_specification_config "cumul"
    end)

  let mk_solver :
      load_path:Api.Files.t -> (module Solving.Utils.SOLVER) * Solving.Utils.env
      =
   fun ~load_path ->
    let open Solving in
    let get_rhs (r : R.partially_typed_rule) =
      match r.rhs with
      | T.Const (_, n) -> B.string_of_ident (B.id n)
      | _ -> raise @@ Cmd_error (WrongConfiguration (E.Rules (B.dloc, [r])))
    in
    let find_lhs opt r =
      match r.R.pat with
      | Pattern (_, n, _) -> B.string_of_ident (B.id n) = opt
      | _ -> false
    in
    let options = try Hashtbl.find config "solver" with _ -> [] in
    let find key default =
      try get_rhs @@ List.find (find_lhs key) options with _ -> default
    in
    let smt = find "smt" "z3" in
    let logic = find "logic" "qfuf" in
    let opt = find "opt" "uf" in
    let (module SS : Utils.SMTSOLVER) =
      if smt = "z3" then
        let open Z3cfg in
        if logic = "lra" then
          let (module R : L.LRA_REIFICATION) = mk_lra_reification () in
          (module Make (Arith (L.MakeLraSpecif (R))))
        else if logic = "qfuf" then (module Make (Syn))
        else raise @@ Cmd_error (Misc "Wrong solver specification: logic")
      else raise @@ Cmd_error (Misc "Wrong solver specification: smt")
    in
    let (module S : Utils.SOLVER) =
      if opt = "uf" then (module Solver.MakeUF (SS))
      else if opt = "normal" then (module Solver.Make (SS))
      else raise @@ Cmd_error (Misc "Wrong solver specification: opt")
    in
    let open Utils in
    let min = int_of_string (find "minimum" "1") in
    let max = int_of_string (find "maximum" "6") in
    let print = find "print" "false" = "true" in
    let mk_theory = mk_smt_theory ~load_path in
    let env = {min; max; print; mk_theory} in
    ((module S : Utils.SOLVER), env)
end

(** [elaborate file] generates two new files [file'] and [file_univ].
    [file'] is the same as [file] except that all universes are replaced by fresh variables.
    [file_univ] contains the declaration of these variables. Everything is done modulo the logic. *)
let elaborate : load_path:Api.Files.t -> string -> unit =
 fun ~load_path in_path ->
  L.log_univ "[ELAB] %s" (F.get_out_path in_path `Elaboration);
  let in_file = F.get_out_path in_path `Input in
  let env = Cmd.to_elaboration_env ~load_path in_file in
  let entries = P.from_file ~file:in_file |> P.to_seq_exn |> List.of_seq in
  (* This steps generates the fresh universe variables *)
  let entries' = List.map (E.mk_entry env) entries in
  (* Write the elaborated terms in the normal file (in the output directory) *)
  let out_file = F.out_from_string in_path `Output in
  let out_fmt = F.fmt_of_file out_file in
  (* The elaborated file depends on the out_sol_md file that contains solution. If the mode is JustElaborate, then this file is empty and import the declaration of the fresh universes *)
  F.add_requires out_fmt
    [F.md_of in_path `Elaboration; F.md_of in_path `Solution];
  List.iter (Api.Pp.Default.print_entry out_fmt) entries';
  F.close out_file;
  F.close env.file;
  F.export in_path ~load_path `Elaboration

(** [check ~load_path file] type checks the file [file] and write the generated constraints in the file [file_cstr]. ASSUME that [file_univ] has been generated previously.
    ASSUME also that the dependencies have been type checked before. *)
let check : load_path:Api.Files.t -> string -> unit =
 fun ~load_path in_path ->
  L.log_univ "[CHECKING] %s" (F.get_out_path in_path `Output);
  let file = F.get_out_path in_path `Output in
  let universo_env = Cmd.to_checking_env ~load_path in_path in
  let requires_mds =
    let deps = C.get_deps () in
    let elab_dep = F.md_of in_path `Elaboration in
    if List.mem elab_dep deps then deps else elab_dep :: deps
  in
  F.add_requires (F.fmt_of_file universo_env.out_file) requires_mds;
  let module P = struct
    type t = unit

    let handle_entry env entry =
      Checking.Checker.mk_entry universo_env env entry

    let get_data _ = ()
  end in
  let hook =
    Api.Processor.
      {
        before =
          (fun env ->
            let sg = Api.Env.get_signature env in
            S.import_signature sg (Cmd.mk_theory ~load_path));
        after =
          (fun _ -> function
            | None -> ()
            | Some (env, loc, exn) ->
                let file = Api.Env.get_filename env in
                Api.Errors.fail_exn ~file loc exn);
      }
  in
  Api.Processor.T.handle_files ~hook ~load_path ~files:[file] (module P);
  Api.Env.export universo_env.env;
  C.flush ();
  F.close universo_env.out_file;
  F.export in_path ~load_path `Checking;
  F.export in_path ~load_path `Solution;
  F.export in_path ~load_path `Output

(** [solve files] call a SMT solver on the constraints generated for all the files [files].
    ASSUME that [file_cstr] and [file_univ] have been generated for all [file] in [files]. *)
let solve : load_path:Api.Files.t -> string list -> unit =
 fun ~load_path in_paths ->
  let (module S : Solving.Utils.SOLVER), env = Cmd.mk_solver ~load_path in
  let add_constraints in_path =
    L.log_univ "[PARSE] %s" (F.get_out_path in_path `Checking);
    S.parse in_path
  in
  List.iter add_constraints in_paths;
  L.log_univ "[SOLVING CONSTRAINTS...]";
  let i, model = S.solve ~load_path env in
  L.log_univ "[SOLVED] Solution found with %d universes." i;
  S.print_model ~load_path (Cmd.output_meta_cfg ~load_path) model in_paths

let simplify : load_path:Api.Files.t -> string list -> unit =
 fun ~load_path in_paths ->
  B.Debug.enable_flag M.debug_flag;
  let normalize_file out_cfg in_path =
    let solution_rules =
      let path = F.get_out_path in_path `Solution in
      M.parse_meta_files [path]
    in
    M.add_rules out_cfg solution_rules;
    let file = F.get_out_path in_path `Output in
    let input = P.from_file ~file in
    let env = Api.Env.init ~load_path ~input in
    let output = F.out_from_string in_path `Simplify in
    let fmt = F.fmt_of_file output in
    let mk_entry e =
      match e with
      | Parsers.Entry.Require (_, _) -> ()
      | e ->
          Format.fprintf fmt "%a@." Api.Pp.Default.print_entry
            (M.mk_entry out_cfg env e)
    in
    P.to_seq_exn input |> Seq.iter mk_entry;
    P.close input;
    F.close output
  in
  let out_cfg = Cmd.output_meta_cfg ~load_path in
  List.iter (normalize_file out_cfg) in_paths

(** [run_on_file ~load_path file] process steps 1 and 2 (depending the mode selected on [file] *)
let run_on_file ~load_path file =
  match !mode with
  | Normal -> elaborate ~load_path file; check ~load_path file
  | JustElaborate -> elaborate ~load_path file
  | JustCheck -> check ~load_path file
  | JustSolve -> ()
  | Simplify -> ()

(* FIXME: Use cmdliner *)
let cmd_options =
  [
    ( "-q",
      Arg.Unit (fun () -> Api.Env.set_debug_mode "q"),
      " Quiet mode (equivalent to -d 'q'" );
    ( "-o",
      Arg.String
        (fun s ->
          F.mk_dir F.output_directory s;
          Api.Files_legacy.add_path s),
      " (MANDATORY) Set the output directory" );
    ( "--theory",
      Arg.String
        (fun s ->
          F.mk_theory s;
          U.md_theory := Api.Files.md s),
      " (MANDATORY) Theory file" );
    ( "--config",
      Arg.String (fun s -> Cmd.config_path := s),
      " (MANDATORY) Configuration file" );
    ( "-l",
      Arg.Unit (fun () -> L.enable_universo_flag ()),
      " Active the debug flag specific to universo" );
    ( "-d",
      Arg.String L.enable_flag,
      " flags enables debugging for all given flags" );
    ( "--elab-only",
      Arg.Unit (fun _ -> mode := JustElaborate),
      " only elaborate files" );
    ( "--check-only",
      Arg.Unit (fun _ -> mode := JustCheck),
      " only generate constraints" );
    ( "--solve-only",
      Arg.Unit (fun _ -> mode := JustSolve),
      " only solves the constraints" );
    ( "--simplify",
      Arg.String
        (fun s ->
          mode := Simplify;
          F.mk_dir F.simplify_directory s),
      " output is simplified so that only usual dk files remain" );
    ( "-I",
      Arg.String Api.Files_legacy.add_path,
      " DIR Add the directory DIR to the load path" );
  ]

(** [generate_empty_sol_file file] generates the file [file_sol] that requires the file [file_univ].
    This is necessary when universo is used with another mode than the Normal one (see elaboration). *)
let generate_empty_sol_file : string -> unit =
 fun in_path ->
  let sol_file = F.get_out_path in_path `Solution in
  let check_md = Api.Files.md (F.get_out_path in_path `Checking) in
  let oc = open_out sol_file in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "#REQUIRE %a.@.@." Api.Pp.Default.print_mident check_md;
  close_out oc

let _ =
  try
    let options = Arg.align cmd_options in
    let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
    let usage = usage ^ "Available options:" in
    let files =
      let files = ref [] in
      Arg.parse options (fun f -> files := f :: !files) usage;
      Cmd.parse_config ();
      List.rev !files
    in
    let load_path =
      Api.Files.(List.fold_left add_path empty @@ Api.Files_legacy.get_path ())
    in
    if !mode <> Simplify then List.iter generate_empty_sol_file files;
    List.iter (run_on_file ~load_path) files;
    if !mode = Normal || !mode = JustSolve then solve ~load_path files;
    if !mode = Simplify then simplify ~load_path files
  with
  (* FIXME: ugly, should be handled by universo via processor. *)
  | (Kernel.Signature.Signature_error _ | Kernel.Typing.Typing_error _) as e ->
      let file = "unknown" in
      Api.Errors.fail_exn ~file Kernel.Basic.dloc e
  | Cmd.Cmd_error (Misc s) ->
      Api.Errors.fail_exit ~code:"-1" ~file:"" None "%s@." s
  | Sys_error err ->
      Format.eprintf "ERROR %s.@." err;
      exit 1
  | Exit -> exit 3
  | Solving.Utils.NoSolution ->
      L.error "Universo found no solution";
      exit 2
