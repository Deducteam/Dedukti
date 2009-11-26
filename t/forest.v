Inductive tree (A B: Set) : Set :=
  leaf : A -> tree A B
| node : B -> forest A B -> tree A B
with forest (A B : Set) : Set :=
  empty : forest A B
| conc : tree A B -> forest A B -> forest A B.