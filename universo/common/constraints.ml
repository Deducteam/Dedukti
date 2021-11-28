module B = Kernel.Basic
module F = Files
module R = Kernel.Rule
module S = Kernel.Signature
module T = Kernel.Term
module U = Universes
module M = Api.Meta

type t = {file : F.cout F.t; meta : M.cfg}

type print_cstrs = {
  eqvar : (B.name * B.name) list;
  axiom : (U.univ * U.univ) list;
  cumul : (U.univ * U.univ) list;
  rule : (U.univ * U.univ * U.univ) list;
}

let dummy_name =
  R.Gamma (false, B.mk_name (B.mk_mident "dummy") (B.mk_ident "dummy"))

let mk_rule vl vr =
  let pat = R.Pattern (B.dloc, vl, []) in
  let rhs = T.mk_Const B.dloc vr in
  R.{ctx = []; pat; rhs; name = dummy_name}

let print_rule pp fmt (l, r) = Format.fprintf fmt "@.[] %a --> %a" pp l pp r

let print_eq_var fmt (l, r) =
  Format.fprintf fmt "%a.@." (print_rule Api.Pp.Default.print_name) (l, r)

let print_predicate fmt p =
  let l' = U.term_of_pred p in
  let r' = U.true_ () in
  Format.fprintf fmt "%a.@." (print_rule Api.Pp.Default.print_term) (l', r')

(** [mk_var_cstre env f l r] add the constraint [l =?= r]. Call f on l and r such that
    l >= r. *)
let mk_var_cstr f l r =
  (* FIXME: is it really necessary to compare number and not the full string ? *)
  let get_number s = int_of_string (String.sub s 1 (String.length s - 1)) in
  let il = B.string_of_ident @@ B.id l in
  let ir = B.string_of_ident @@ B.id r in
  let nl = get_number il in
  let nr = get_number ir in
  if nl = 0 && nr = 0 then if r < l then (f l r; (l, r)) else (f r l; (r, l))
  else if nr < nl then (f l r; (l, r))
  else (f r l; (r, l))

let deps = ref []

let mk_cstr env f cstr =
  let fmt = F.fmt_of_file env.file in
  match cstr with
  | U.Pred p       ->
      Format.fprintf fmt "%a@." print_predicate p;
      true
  | U.EqVar (l, r) ->
      let l, r = mk_var_cstr f l r in
      (* FIXME: explain the rationale. *)
      Api.Meta.add_rules env.meta [mk_rule l r];
      if not (List.mem (B.md r) !deps) then deps := B.md r :: !deps;
      Format.fprintf fmt "%a@." print_eq_var (l, r);
      true

let get_deps () = !deps

let flush () = deps := []
