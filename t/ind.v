(* Simple inductive test with parameters + indices. *)
Inductive e (A : Type) (x : A) : A -> Type :=
  refl : e A x x.

(* Inductive type with parameters. *)
Inductive o (A : Type) : Type :=
  J : forall x : A, o A
| N : o A.

(* Silly inductive type. *)
Inductive i : Type := A | B.

(* Recursive inductive type with parameter. *)
Inductive r (A : Type) : Type :=
  S : A -> r A -> r A
| Z : r A.
