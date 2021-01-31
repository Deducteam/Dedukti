module B = Kernel.Basic
module L = Common.Logic
module U = Common.Universes
module ZA = Z3.Arithmetic
module ZB = Z3.Boolean
module ZI = ZA.Integer

module Make (Spec : L.LRA_SPECIFICATION) = struct
  type t = Z3.Expr.expr

  type smt_model = Z3.Model.model

  type ctx = Z3.context

  let logic = `Lra

  let mk_name : B.name -> string =
   fun name -> B.string_of_mident (B.md name) ^ B.string_of_ident (B.id name)

  let int_sort ctx = ZI.mk_sort ctx

  let mk_var : ctx -> string -> t =
   fun ctx s -> Z3.Expr.mk_const_s ctx s (int_sort ctx)

  let to_int : ctx -> int -> t = fun ctx i -> ZI.mk_numeral_i ctx i

  let mk_univ : ctx -> U.univ -> t =
   fun ctx u ->
    match u with
    | Sinf -> to_int ctx (-1)
    | Var name -> mk_var ctx (mk_name name)
    | Enum n -> to_int ctx n

  let mk_max : ctx -> t -> t -> t =
   fun ctx l r -> ZB.mk_ite ctx (ZA.mk_le ctx l r) r l

  let mk_imax : ctx -> t -> t -> t =
   fun ctx l r ->
    ZB.mk_ite ctx
      (ZB.mk_eq ctx r (to_int ctx 0))
      (to_int ctx 0) (mk_max ctx l r)

  let mk : type a. (ctx, a) L.op -> (a, t) L.arrow =
   fun op ->
    match op with
    | L.True ctx -> L.Zero (ZB.mk_true ctx)
    | L.False ctx -> L.Zero (ZB.mk_false ctx)
    | L.Zero ctx -> L.Zero (to_int ctx 0)
    | L.Succ ctx -> L.One (fun a -> ZA.mk_add ctx [ a; to_int ctx 1 ])
    | L.Minus ctx -> L.One (fun a -> ZA.mk_unary_minus ctx a)
    | L.Eq ctx -> L.Two (ZB.mk_eq ctx)
    | L.Max ctx -> L.Two (mk_max ctx)
    | L.IMax ctx -> L.Two (mk_imax ctx)
    | L.Le ctx -> L.Two (ZA.mk_le ctx)
    | L.Ite ctx -> L.Three (ZB.mk_ite ctx)

  let mk = { L.mk }

  let mk_axiom = Spec.mk_axiom mk

  let mk_rule = Spec.mk_rule mk

  let mk_cumul = Spec.mk_cumul mk

  let mk_bounds ctx string up =
    let right =
      if up > 0 then ZA.mk_le ctx (mk_var ctx string) (to_int ctx (up - 1))
      else ZB.mk_true ctx
    in
    let left = ZA.mk_le ctx (to_int ctx 0) (mk_var ctx string) in
    ZB.mk_and ctx [ left; right ]

  let solution_of_var : ctx -> int -> Z3.Model.model -> string -> U.univ =
   fun ctx _ model var ->
    match Z3.Model.get_const_interp_e model (mk_var ctx var) with
    | None ->
        Format.eprintf "%s@." var;
        assert false
    | Some e ->
        let v = Z.to_int (ZI.get_big_int e) in
        U.Enum v

  let mk_theory = false
end
