Module Type M.
Parameter n : Type.
End M.

Module E (Mo : M).
Definition t := Mo.n.
End E.

Module F.
Definition n := Prop.
End F.

Module G := E F.
