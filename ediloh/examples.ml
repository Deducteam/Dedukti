open Opentheory

let var_type str = mk_varType (mk_name [] "A")

let id_term ty =
  let var = mk_name [] "x" in
  let v = mk_var var ty in
  mk_absTerm (mk_var var ty) (mk_varTerm v)

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
  let term,hyp,thm = mk_axiom_true in
  mk_thm term hyp thm

let proof_axiom_and () =
  let term,hyp,thm = mk_axiom_and (equal_term mk_true_term mk_true_type) mk_true_term in
  mk_thm term hyp thm

let proof_axiom_impl () =
  let term,hyp,thm = mk_axiom_impl (equal_term mk_true_term mk_true_type) mk_true_term in
  mk_thm term hyp thm

let proof_axiom_forall () =
  let term,hyp,thm = mk_axiom_forall (id_term mk_bool_type) mk_bool_type in
  mk_thm term hyp thm

let test () = proof_axiom_forall ()
