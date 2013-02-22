Module F.
  Module H. Definition t := Prop. End H.
End F.

Module G := F.

Definition x := G.H.t.
