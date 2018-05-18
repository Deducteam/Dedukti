type model = Basic.ident -> Term.term

module type Solver =
sig
  type expr

  val mk_var  : string -> expr

  val mk_prop : expr

  val mk_type : int -> expr

  val mk_succ : expr -> expr

  val mk_max : expr -> expr -> expr

  val mk_rule : expr -> expr -> expr

  val mk_eq : expr -> expr -> unit

  val solve : unit -> int * model

  val reset : unit -> unit
end

val univ_max : int ref

module Z3Syn : Solver
(*
module Z3LRA : Solver
*)
