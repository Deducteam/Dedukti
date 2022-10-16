module B = Kernel.Basic
module M = Api.Meta

(** This module declares all the types and signatures needed to implement an smt solver. *)

(** Qfuf is a logic with non-interpreted symbols, can be slow.
    Lra is linear arithmetic. Fast, but it requires an interpretation of CTS specification into linear arithmetic.
 *)
type logic = [`Qfuf | `Lra]

(** Any SMT Logic *)
module type LOGIC = sig
  (** Type for formulas *)
  type t

  (** Model returns by the solver *)
  type smt_model

  (** Meta informations needed by the solver *)
  type ctx

  (** Specify the logic implemented *)
  val logic : logic

  (** [mk_name n] returns a SMT string from a Dedukti name [n] *)
  val mk_name : B.name -> string

  (** [mk_var ctx s] returns a variable expression as to the name [s] *)
  val mk_var : ctx -> string -> t

  (** [mk_univ ctx u] returns an expression associated to the universe [u] *)
  val mk_univ : ctx -> Common.Universes.univ -> t

  (** [mk_axiom ctx s s'] returns an expression encoding the predicate A(s,s') *)
  val mk_axiom : ctx -> t -> t -> t

  (** [mk_cumul ctx s s'] returns an expression encoding the predicate C(s,s') *)
  val mk_cumul : ctx -> t -> t -> t

  (** [mk_rule ctx s s' s''] returns an expression encoding the predicate R(s,s',s'') *)
  val mk_rule : ctx -> t -> t -> t -> t

  (** [mk_bound ctx s i] returns an expression encoding that a variable cannot be higher that the ith universe *)
  val mk_bounds : ctx -> string -> int -> t

  (** [solution_of_var ctx i model s] returns the universe found by the SMT solver *)
  val solution_of_var :
    ctx -> int -> smt_model -> string -> Common.Universes.univ
end

(** configures the SMT solver *)
type env = {
  mk_theory : int -> Common.Oracle.theory;
      (** Compute a theory related to the target CTS specification. *)
  min : int;  (** minimum number of universes to check *)
  max : int;  (** maximum number of universes to check *)
  print : bool;  (** print the problem in a file *)
}

(** [model] is a function that associate to each fresh universe a concrete universe. *)
type model = B.name -> Common.Universes.univ

(** An SMT solver is a solver specific to one SMT such as Z3 *)
module type SMTSOLVER = sig
  (** [add pred] add the predicate [cstr] to the solver  *)
  val add : Common.Universes.cstr -> unit

  (** [solve mk_theory] call the solver and returns the mimimum number of universes needed to solve the constraints as long as the model. The theory used by solver depends on the number of universes needed. Hence one needs to provide a function [mk_theory] that builds a theory when at most [i] are used.*)
  val solve : load_path:Api.Files.t -> env -> int * model
end

(** Generic solver for Universo *)
module type SOLVER = sig
  (** [parse file] parses the constraint associated to [file] *)
  val parse : Common.Files.path -> unit

  (** [print_model meta model file] prints the model into the solution file associated to [file]. Universes are translated to terms via the [meta] rules. *)
  val print_model :
    load_path:Api.Files.t -> M.cfg -> model -> Common.Files.path list -> unit

  (** [solve env] solves all the files parsed and returns a model as long as [i], the number of universes needed. As postconditions, [i >= env.minimum && i <= maximum]. Moreover forall [j < i], the solver did not found a solution. *)
  val solve : load_path:Api.Files.t -> env -> int * model
end

exception NoSolution
