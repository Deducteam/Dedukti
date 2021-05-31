module B = Kernel.Basic
module C = Common.Constraints
module E = Parsers.Entry
module F = Common.Files
module L = Common.Log
module M = Api.Meta
module O = Common.Oracle
module P = Parsers.Parser
module R = Kernel.Rule
module S = Kernel.Signature
module T = Kernel.Term
module U = Common.Universes
open Utils

(** [from_rule pat rhs] add the assertion [pat = rhs]. *)
let from_rule : R.pattern -> T.term -> U.cstr =
 fun pat right ->
  let left = R.pattern_to_term pat in
  try (* the constraint is a predicate *)
      U.Pred (U.extract_pred left)
  with U.Not_pred ->
    (* the constraint is en equality between variables *)
    let left' = Elaboration.Var.name_of_uvar left in
    let right' = Elaboration.Var.name_of_uvar right in
    U.EqVar (left', right')

module Make (Solver : SMTSOLVER) : SOLVER = struct
  (** [parse meta s] parses a constraint file. *)
  let parse : string -> unit =
   fun in_path ->
    let module P = struct
      type t = U.cstr list

      let cstrs = ref []

      let handle_entry _ = function
        | E.Rules (_, rs) ->
            cstrs :=
              List.map
                (fun (r : R.partially_typed_rule) -> from_rule r.pat r.rhs)
                rs
              :: !cstrs
        | E.Require _ -> ()
        | _ -> assert false

      let get_data _ = List.flatten !cstrs
    end in
    let cstr_file = F.get_out_path in_path `Checking in
    let cstrs = Api.Processor.T.handle_files [ cstr_file ] (module P) in
    List.iter Solver.add cstrs

  (** [print_model meta model f] print the model associated to the universes elaborated in file [f]. Each universe are elaborated to the original universe theory thanks to the dkmeta [meta] configuration. *)
  let print_model meta model in_path =
    let elab_file = F.get_out_path in_path `Elaboration in
    let sol_file = F.out_from_string in_path `Solution in
    let fmt = F.fmt_of_file sol_file in
    let md_theory = P.md_of_file (F.get_theory ()) in
    F.add_requires fmt [ F.md_of in_path `Elaboration; md_theory ];
    let module P = struct
      type t = unit

      let handle_entry env =
        let (module Printer) = Api.Env.get_printer env in
        function
        | E.Decl (_, id, _, _, _) ->
            let name = B.mk_name (Api.Env.get_name env) id in
            let sol = model name in
            let rhs = U.term_of_univ sol in
            let rhs' = M.mk_term meta rhs in
            Format.fprintf fmt "[] %a --> %a.@." Printer.print_name name
              Printer.print_term rhs'
        | _ -> ()

      let get_data _ = ()
    end in
    Api.Processor.T.handle_files [ elab_file ] (module P);
    F.close sol_file

  let print_model meta model files = List.iter (print_model meta model) files

  let solve = Solver.solve
end

(** Performance are bad with LRA *)
module MakeUF (Solver : SMTSOLVER) : SOLVER = struct
  module SP = Set.Make (struct
    type t = U.pred

    let compare = compare
  end)

  let env = Api.Env.init (P.input_from_string (B.mk_mident "solver") "")

  let sp = ref SP.empty

  let mk_rule : R.partially_typed_rule -> unit =
   fun r ->
    let sg = Api.Env.get_signature env in
    match from_rule r.pat r.rhs with
    | U.EqVar _ -> S.add_rules sg [ R.to_rule_infos r ]
    | U.Pred p -> sp := SP.add p !sp

  (** [parse meta s] parses a constraint file. *)
  let parse : string -> unit =
   fun in_path ->
    let module P = struct
      type t = unit

      let handle_entry _ = function
        | E.Rules (_, rs) -> List.iter mk_rule rs
        | E.Require _ -> ()
        | _ -> assert false

      let get_data _ = ()
    end in
    let cstr_file = F.get_out_path in_path `Checking in
    Api.Processor.T.handle_files [ cstr_file ] (module P)

  (* List.iter S.add entries' *)
  (* TODO: This should be factorized. the normalization should be done after solve and return a correct model *)

  (** [print_model meta model f] print the model associated to the universes elaborated in file [f]. Each universe are elaborated to the original universe theory thanks to the dkmeta [meta] configuration. *)
  let print_model meta_constraints meta_output model in_path =
    let elab_file = F.get_out_path in_path `Elaboration in
    let sol_file = F.out_from_string in_path `Solution in
    let fmt = F.fmt_of_file sol_file in
    let md_theory = P.md_of_file (F.get_theory ()) in
    F.add_requires fmt [ F.md_of in_path `Elaboration; md_theory ];
    let module P = struct
      type t = unit

      let handle_entry env =
        let (module Printer) = Api.Env.get_printer env in
        let find name =
          match M.mk_term meta_constraints (T.mk_Const B.dloc name) with
          | T.Const (_, name) -> name
          | _ -> assert false
        in
        function
        | E.Decl (_, id, _, _, _) ->
            let name = B.mk_name (Api.Env.get_name env) id in
            let sol = model (find name) in
            let rhs = U.term_of_univ sol in
            let rhs' = M.mk_term meta_output rhs in
            Format.fprintf fmt "[] %a --> %a.@." Printer.print_name name
              Printer.print_term rhs'
        | _ -> ()

      let get_data _ = ()
    end in
    Api.Processor.T.handle_files [ elab_file ] (module P);
    F.close sol_file

  let print_model meta model files =
    let cstr_files =
      List.map (fun file -> F.get_out_path file `Checking) files
    in
    let meta_constraints = M.meta_of_files cstr_files in
    List.iter (print_model meta_constraints meta model) files

  let solve solver_env =
    let meta = { M.default_config with env } in
    let normalize : U.pred -> U.pred =
     fun p -> U.extract_pred (M.mk_term meta (U.term_of_pred p))
    in
    L.log_solver "[NORMALIZE CONSTRAINTS...]";
    let sp' = SP.map normalize !sp in
    L.log_solver "[NORMALIZE DONE]";
    SP.iter (fun p -> Solver.add (Pred p)) sp';
    Solver.solve solver_env
end
