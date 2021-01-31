module B = Kernel.Basic
module L = Common.Log
module U = Common.Universes
module Z = Z3
module ZS = Z.Solver
open Utils

(** Concrete configuration. *)
let cfg =
  [
    `Model true;
    (* Generate a model *)
    `Proof false;
    (* Give a proof if unsatisfiable *)
    `Trace false;
    (* Do not generate trace *)
  ]

let string_of_cfg_item item =
  match item with
  | `Model b -> ("model", string_of_bool b)
  | `Proof b -> ("proof", string_of_bool b)
  | `Trace b -> ("trace", string_of_bool b)
  | `TraceFile file -> ("trace_file_name", file)

let string_of_cfg cfg = List.map string_of_cfg_item cfg

(** Z3 context elaborated from a Z3 configuration *)
let ctx = Z.mk_context (string_of_cfg cfg)

module type Z3LOGIC =
  Utils.LOGIC
    with type t = Z.Expr.expr
     and type smt_model = Z.Model.model
     and type ctx = Z.context

module Make (ZL : Z3LOGIC) = struct
  (** Set containing all the variables used by Z3 *)
  module SSet = Set.Make (struct
    type t = string

    let compare = compare
  end)

  (* Set of Z3 variables *)
  let vars = ref SSet.empty

  (* Z3 Solver *)
  let solver = Z3.Solver.mk_simple_solver ctx

  (** [add expr] add the asserition [expr] in the Z3 solver. [expr] should be a predicate. *)
  let add expr = Z3.Solver.add solver [ expr ]

  (** [mk_var s] construct a Z3 expression from the Z3 variable [s]. *)
  let mk_var s =
    vars := SSet.add s !vars;
    ZL.mk_var ctx s

  let vars_of_univs univs =
    let f = function
      | U.Var name -> vars := SSet.add (ZL.mk_name name) !vars
      | _ -> ()
    in
    List.iter f univs

  let vars_of_pred = function
    | U.Axiom (s, s') -> vars_of_univs [ s; s' ]
    | U.Cumul (s, s') -> vars_of_univs [ s; s' ]
    | U.Rule (s, s', s'') -> vars_of_univs [ s; s'; s'' ]

  (** [mk_pred p] construct the Z3 predicate from a universe predicate *)
  let mk_pred p =
    (* register variables first to add bounds constraints afterward *)
    vars_of_pred p;
    match p with
    | U.Axiom (s, s') -> ZL.mk_axiom ctx (ZL.mk_univ ctx s) (ZL.mk_univ ctx s')
    | U.Cumul (s, s') -> ZL.mk_cumul ctx (ZL.mk_univ ctx s) (ZL.mk_univ ctx s')
    | U.Rule (s, s', s'') ->
        ZL.mk_rule ctx (ZL.mk_univ ctx s) (ZL.mk_univ ctx s')
          (ZL.mk_univ ctx s'')

  (** [mk_theory m] construct a Z3 theory for the non-interpreted predicate using the theory [t]. *)
  let mk_theory t =
    List.iter
      (fun (p, b) ->
        if b then add (mk_pred p) else add (Z.Boolean.mk_not ctx (mk_pred p)))
      t

  (** [register_vars vars i] give bound for each variable [var] between [0] and [i] *)
  let register_vars vars i =
    SSet.iter (fun var -> add (ZL.mk_bounds ctx var i)) vars

  (** [mk_cstr c] construct the Z3 constraint from the universe constraint [c] *)
  let mk_cstr c =
    let open U in
    match c with
    | Pred p -> mk_pred p
    | EqVar (l, r) ->
        Z.Boolean.mk_eq ctx (mk_var (ZL.mk_name l)) (mk_var (ZL.mk_name r))

  (** [check theory_of i] solves the current constraints with at most [i] universes. If no solution is found, [check] is called recursively on [i+1]. *)
  let rec check env i =
    if i > env.max then raise NoSolution;
    ZS.push solver;
    let theory = env.mk_theory i in
    if ZL.logic = `Qfuf then mk_theory theory;
    register_vars !vars i;
    match ZS.check solver [] with
    | ZS.UNSATISFIABLE ->
        L.log_solver "[SOLVER] No solution found with %d universes" i;
        ZS.pop solver 1;
        check env (i + 1)
    | ZS.UNKNOWN -> assert false
    | ZS.SATISFIABLE -> (
        match ZS.get_model solver with
        | None -> assert false
        | Some model ->
            let model (cst : B.name) : U.univ =
              let var = ZL.mk_name cst in
              ZL.solution_of_var ctx i model var
            in
            (i, model) )

  (** [solve mk_theory] tries to solve the constraints *)
  let solve env =
    L.log_solver "[SOLVER] Solving...";
    check env env.min

  let add : U.cstr -> unit = fun cstr -> add (mk_cstr cstr)
end

module Syn : Z3LOGIC = Z3syn

module Arith = Z3arith.Make
