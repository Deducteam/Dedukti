(* Simple inductive test with parameters + indices. *)
Inductive e (A : Type) (x : A) : A -> Type :=
  refl : e A x x.

(* Inductive type with parameters. *)
Inductive o (A : Type) : Type :=
  S : forall x : A, o A
| N : o A.

(* Silly inductive type. *)
Inductive i : Type := A | B.
