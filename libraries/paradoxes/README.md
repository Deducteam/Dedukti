Several encodings of paradoxes.

- The Liar Paradox

Three variants of the liar paradox (the sentense "This sentense is
false" is true if and only if it is false) are presented :

  + in file `liar.dk`, the paradoxical sentense is defined in Deduction
    Modulo by the rewrite rule "[] liar --> liar -> false"

  + in files `yablo.dk` and `yablo2.dk`, we use a variant of the liar
    paradox due to Stephen Yablo that he claims is not circular: for
    each natural number n, he defines the sentense φₙ := ∀ m > n, ¬φₘ
    then it easy to show that
    * ∀n, φₙ → φₙ₊₁
    * ∀n, φₙ → ¬φₙ₊₁
    * ∀n, ¬φₙ
    * φ₁
    * ¬φ₁
    * ⊥

    The difference between both files resides in the definition of the
    order relation < on natural numbers : in file `yablo.dk` it is
    defined recursively whereas in file `yablo2.dk` it is replaced by
    addition which leads to a slightly smaller formalisation.

- Burali-Forti / Girard paradox

Burali-Forti is among the first (if not the first) paradoxes
discovered in set theory and Girard paradox, its adaptation to type
theory, is the first paradox discovered in type theory. We present two
formalisations of Girard paradox, the first one comes from the 1972
presentation by Per Martin-Löf of Intuitionistic Type Theory and shows
the inconsistency of the type theory with `Type : Type` and the
following type constructions : 0, Π, Σ, and ℕ. The second is a Dedukti
port of an Agda formalisation of this paradox due to favonia, it
avoids the use of natural numbers.

- Hurkens paradox

Hurkens paradox is the shortest known term of type false in System
U⁻. We wrote this term in the Dedukti encoding of System U⁻ in file
`hurkens.dk`.

- Russell Paradox

Russell paradox is the simplest paradox of set theory: let R be the
set {x | x ∉ x}, we have R ∈ R if and only if R ∉ R.  We provide
several encodings of Russell paradox: the first one in file
`russell.dk` encodes sets as trees with arbitrary branching using an
impredicative inductive type; I do not know who discovered this
version of Russell paradox in type theory first. The second one in
file `miquel.dk` is due to Alexandre Miquel, it is expressed in System
U⁻ and encodes sets as pointed graphs quotiented by bisimulation.
Four versions of Russell Paradox in Deduction Modulo due to Gilles
Dowek and Benjamin Werner are also presented; the simplest one is
`dowek_werner_russell.dk` which is very close to the Liar paradox in
`liar.dk`, then Crabbés conterexample to the termination of
cut-elimination in ZF is presented first as a logical paradox in file
`dowek_werner_crabbe.dk` and then in ZF in file
`dowek_werner_crabbe_zf.dk`. Finally, the file
`dowek_werner_russell_terminating.dk` presents an encoding of Russell
paradox in Deduction Modulo a convergent rewrite system.
