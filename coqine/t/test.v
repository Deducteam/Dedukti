Axiom A : Set.

Definition id_A := fun x: A => x.

Definition morph_A :=  A -> A.

Theorem p : morph_A.
exact id_A.
Qed.
