Section Streams.

Variable A : Type.

Inductive nat := O | S : nat-> nat.

Inductive eq (A : Type) (x : A) : A -> Prop := refl : eq A x x.

CoInductive Stream : Type :=
    Cons : A -> Stream -> Stream.

Definition hd (x:Stream) := match x with
                            | Cons a _ => a
                            end.

Definition tl (x:Stream) := match x with
                            | Cons _ s => s
                            end.

Fixpoint Str_nth_tl (n:nat) (s:Stream) {struct n} : Stream :=
  match n with
  | O => s
  | S m => Str_nth_tl m (tl s)
  end.

Definition Str_nth (n:nat) (s:Stream) : A := hd (Str_nth_tl n s).

Lemma unfold_Stream :
 forall x:Stream,  eq Stream x  (match x with
                      | Cons a s => Cons a s
                      end).
Proof.
  intro x.
  case x.
  intros.
exact (refl Stream (Cons a s)).
Qed.

End Streams.