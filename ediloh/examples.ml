open Openstt

open OpenTheory

let var_type str = mk_varType (mk_name [] "A")

let id_term ty =
  let var = mk_name [] "x" in
  let v = mk_var var ty in
  mk_abs_term (mk_var var ty) (mk_var_term v)

let equal_term t ty =
  mk_equal_term t t ty

let proof_refl t ty =
  let thm =  mk_refl t in
  let eq = mk_equal_term t t ty in
  mk_thm eq (mk_hyp []) thm

let proof_refl_id () =
  let ty = var_type "A" in
  proof_refl (id_term ty) (mk_arrow_type ty ty)

let proof_refl_true () =
  proof_refl mk_true_term mk_true_type

let proof_refl_impl_id () =
  proof_refl (mk_impl_term mk_true_term mk_true_term) mk_bool_type

let proof_axiom_true () =
  let thm = mk_axiom_true in
  let term = mk_true_term in
  mk_thm term (mk_hyp []) thm
(*
let proof_axiom_and () =
  let thm = mk_axiom_and (equal_term mk_true_term mk_true_type) mk_true_term in
  let term = mk_and_term (equal_term mk_true_term mk_true_type) mk_true_term in
  mk_thm term (mk_hyp []) thm

let proof_axiom_impl () =
  let term,hyp,thm = mk_axiom_impl (equal_term mk_true_term mk_true_type) mk_true_term in
  mk_thm term hyp thm

let proof_axiom_forall () =
  let term,hyp,thm = mk_axiom_forall (id_term mk_bool_type) mk_bool_type in
  mk_thm term hyp thm
*)

let proof_forall_intro () =
  let ty = mk_bool_type in
  let t = id_term ty in
  let thm =  mk_refl t in
  let eq = mk_equal_term t t (mk_arrow_type ty ty) in
  let thm = mk_rule_intro_forall (mk_name [] "y") (var_type "A") eq thm in
  let term = mk_forall_term (mk_abs_term (mk_var (mk_name [] "y") (var_type "A")) eq) (var_type "A") in
  mk_thm term (mk_hyp []) thm
(*
let proof_forall_elim () =
  let ty = mk_bool_type in
  let t = id_term ty in
  let thm =  mk_refl t in
  let eq = mk_equal_term t t (mk_arrow_type ty ty) in
  let term,hyp,thm = mk_rule_intro_forall (mk_name [] "y") (var_type "A") eq thm in
  let term,hyp,thm = mk_rule_elim_forall thm (mk_absTerm (mk_var (mk_name [] "v") (var_type "B")) eq) (var_type "A") (mk_varTerm (mk_var (mk_name [] "y") (var_type "B"))) in
  mk_thm term hyp thm

let proof_impl_intro () =
  let ax = mk_axiom (mk_hyp [mk_true_term]) (mk_equal_term mk_true_term mk_true_term mk_bool_type) in
  let term,hyp,thm = mk_rule_intro_impl ax mk_true_term (mk_equal_term mk_true_term mk_true_term mk_bool_type) in
  mk_thm term hyp thm

let proof_impl_elim () =
  let ax = mk_axiom (mk_hyp [mk_true_term]) (mk_equal_term mk_true_term mk_true_term mk_bool_type) in
  let term,hyp,thm = mk_rule_intro_impl ax mk_true_term (mk_equal_term mk_true_term mk_true_term mk_bool_type) in
  let _,_,thm_true = mk_axiom_true in
  let term,hyp,thm = mk_rule_elim_impl thm_true thm mk_true_term (mk_equal_term mk_true_term mk_true_term mk_bool_type) in
  mk_thm term hyp thm
*)
let test () = proof_forall_intro ()
