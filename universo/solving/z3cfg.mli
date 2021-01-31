open Utils

module type Z3LOGIC = Utils.LOGIC with type t         = Z3.Expr.expr
                                   and type smt_model = Z3.Model.model
                                   and type ctx       = Z3.context

module Make(ZL:Z3LOGIC) : SMTSOLVER

module Syn  : Z3LOGIC
module Arith(S:Common.Logic.LRA_SPECIFICATION)  : Z3LOGIC
