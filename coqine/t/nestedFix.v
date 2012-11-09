Inductive nat := 
O : nat
| S : nat -> nat.

Definition nested := fix f x := (fix g y := 
match x with O => O 
| S x1 => match y with
  O => f x1
| S y1 => g y1 end end) x. 