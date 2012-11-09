(* Simple inductive test with parameters + indices. *)
Inductive e (A : Type) (x : A) : A -> Type :=
  refl : e A x x.
