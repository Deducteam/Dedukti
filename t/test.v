Module Type Int.
Parameter int : Set.
End Int.

Module MoreInt(I : Int).
Import I.

Inductive E :  Set :=
raw : int -> E.
End MoreInt.

