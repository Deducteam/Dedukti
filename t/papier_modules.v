Module Type T.
Parameter a : Type.
Parameter b : a.
End T.

Module M : T.
Definition a := Prop.
Definition b : Prop := forall p : Prop, p.
End M.

Module N (O : T).
Definition x := O.b.
End N.

Module P := N(M).
