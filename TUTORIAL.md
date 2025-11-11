Dedukti Tutorial
================

Introduction
------------

This is a tutorial for the Dedukti type-checker. It is not a manual for Dedukti (which is available [here](https://github.com/Deducteam/Dedukti/blob/master/README.md)) nor an exhaustive list of Dedukti features.

Compared to similar software such as Coq and Twelf, Dedukti has two main differences:

*   Dedukti is based on rewriting, a very powerful mechanism which can be used to discharge the computational part of proofs to the type-checker.
*   Dedukti is a type-checker, not a proof assistant; it has been designed for checking machine-generated proofs and it does not contain features whose sole purpose is to help human users (tactics and mechanisms for trying to fill gaps automatically for example).

However, since writing a Dedukti generator usually starts with manually defining its logic in Dedukti, it is often necessary to learn how to write Dedukti code directly. This tutorial is a starting point to achieve this.

Installation
------------

### Dedukti Installation

Dedukti installation is described in the [manual](https://github.com/Deducteam/Dedukti/blob/master/README.md). This tutorial has been written for Dedukti version 2.5.

### Emacs Mode

Dedukti code can conveniently be edited using the text editor GNU Emacs. For Emacs version 24 and more, the Emacs mode for Dedukti can be installed through Emacs package manager.

#### Adding the MELPA Repository

Dedukti-mode is hosted on the MELPA repository, to allow MELPA, add the following line to your Emacs configuration file:

```
(add-to-list package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
```

#### Installing Dedukti-mode

To install Dedukti-mode, type the following command:

```bash
M-x package-install <RET> dedukti-mode
```

The `flycheck` tool is aware of Dedukti; it can be installed by the following command:

```bash
M-x package-install <RET> flycheck-dedukti
```

Getting started
---------------

### The `#NAME` Line

To get started, visit (open) a new file with the `.dk` extension, say `foo.dk`. Emacs should select `dedukti-mode` for the buffer major mode and indicate this by the string "Dedukti" in the mode line. If flycheck is enabled, it indicates that the buffer is not accepted by Dedukti with the following error message: `Unexpected token ''.`.

A Dedukti file should always start with a line of the form `#NAME <module_name>.` where `<module_name>` is the name of the module beeing defined by the file. This name will be used to refer to constants and functions defined in the file from other Dedukti developments. If the file is named `foo.dk` then `foo` is a good candidate for the module name.

Add the following line:

```
#NAME foo.
```

### Comments

In Dedukti, comments are enclosed by "(;" and ";)". In Emacs, these can be inserted by typing `M-;`.

Add a comment at the beginning of your file.

Natural Numbers
---------------

### First Declarations and Definitions

To define Peano natural numbers, we need to declare 3 constants:

*   A type `Nat`, the type of natural numbers;
*   A constant `0` of type `Nat`,
*   A constant `S` of type `Nat` → `Nat`, the successor function.

Note that digits are considered regular letters in Dedukti so `0` is a valid identifier.

```
Nat : Type.
0 : Nat.
S : Nat -> Nat.
```

We can define `1` as the successor of `0`, `2` as the successor of `1`, etc…:

```
def 1 := S 0.
def 2 := S 1.
def 3 := S 2.
def 4 := S 3.
```

We can also ask Emacs to generate these definitions for us:

```
M-: (dotimes (i 100) (insert (format "def %d := S %d.\\n" (+ 1 i) i)))
```

### First Rewrite Rules

Peano addition can be defined as follows:

0 + n = n

(m+1) + n = (m + n) + 1

In Dedukti, this recursive definition can be written like this:

```
def plus : Nat -> Nat -> Nat.
[n] plus 0 n --> n
[m,n] plus (S m) n --> S (plus m n).
```

The first line is the declaration of the symbol `plus`, the following lines are rewrite rules defining the function `plus` by pattern-matching on its first argument. The keyword `def` starting the declaration of `plus` indicates that the symbol `plus` is allowed to appear at the head of rewrite rules. The names appearing inside the brackets at the left of each rule are the variables to be substituted for applying the rewrite rule.

Define the multiplication `mult` using the following definition:

0 × n = 0

(m+1) × n = (m × n) + n

Simple Booleans
---------------

### Booleans

As we defined addition, we can define equality of natural numbers but we first need to define booleans:

```
Bool : Type.
True : Bool.
False : Bool.
```

### Equality

Equality of natural numbers is defined by pattern-matching on both arguments, the pattern `_` represents any term which we do not need to name, this pattern can only appear in the left-hand side of rewrite rules:

```
def equal : Nat -> Nat -> Bool.
[] equal 0 0 --> True
[] equal (S _) 0 --> False
[] equal 0 (S _) --> False
[m,n] equal (S m) (S n) --> equal m n.
```

Define a function `leq : Nat -> Nat -> Bool` such that `leq n m` is `True` if `n` ≤ `m` and `False` if `n` > `m`.

### Testing

In order to test our function, we want to check that `mult` 10 10 is equal to 100.

This means that the term `equal (mult 10 10) 100` reduces to the normal form `True`.

For a quick test, we can ask Emacs and Dedukti to evaluate a term; if you select the term `equal (mult 10 10) 100` anywhere after the definitions of `mult` and `equal`, even in a comment, you can run the Emacs command `M-x dedukti-snf` which prints the normal form of the selected term in the echo area.

The commands dedukti-wnf, dedukti-hnf and dedukti-step are similar but print respectively the weak normal form, the head-normal-form and the term reduced by just one step.

Check that the strong normal form of `equal (mult 10 10) 100` is True.

Test your `leq` function on the following examples:

*   `leq 50 100` ↪* `True`
*   `leq 100 50` ↪* `False`
*   `leq 100 100` ↪* `True`

### Testing by type-checking with a dependent type

If you don't want to use Emacs, it is also possible to use Dedukti type-checking to check if a boolean is `True`:

```
Istrue : Bool -> Type.
tt : Istrue True.
def test1 : Istrue (equal (mult 10 10) 100) := tt.
```

Istrue is neither a type like `Nat` and `Bool` nor a function, it is a family of types indexed by a boolean; this is called a **dependent type**.

There is no term of type `Istrue False` and `tt` is the only term of type `Istrue True`.

The line `def test1 : Istrue (equal (mult 10 10) 100) :` `tt.` defines the symbol `test1` of type `Istrue (equal (mult 10 10) 100)` by the term `tt`. Dedukti will accept this definition only if `test1` and `tt` have the same type, so this definition will be accepted only if `equal (mult 10 10) 100` and `True` are convertible terms.

Lists
-----

Lists over a fixed type `A` can be defined very similarly to natural numbers:

```
List : Type.
Nil : List.
Cons : A -> List -> List.
```

Here is an example of a function working on lists:

```
def append : List -> List -> List.
[ l2 : List ] append Nil l2 --> l2
[ a : A, l1 : List, l2 : List ] append (Cons a l1) l2 --> Cons a (append l1 l2).
```

Define a function `length` with the following signature:

```
def length : List -> Nat.
```

such that `length l` reduces to the number of elements of the list `l`.

Partial Functions
-----------------

Dedukti doesn't check for exhaustivity of pattern-matching; this allows us to write partial functions like the predecessor function on natural numbers:

```
def pred : Nat -> Nat.
[ n : Nat ] pred (S n) --> n.
```

The term `pred 0` is a well-typed term of type `Nat` which is neither `0` nor of the form `S n`. Hence it won't be useful in computations.

Define two functions `head` and `tail` with the following signatures:

```
def head : List -> A.
def tail : List -> List.
```

`head l` should reduce to the first element of the list `l` and `tail l` to the list compsed of the other elements.

As we will see in [9](#orgheadline19), it is also possible to define lists in a way that forbids the terms `tail Nil` and `head Nil`.

Integers and Smart Constructors
-------------------------------

Integers may be defined by differences of natural numbers, using a single constructor `Diff` of type `Nat` → `Nat` → `Int`.

```
Int : Type.
def Diff : Nat -> Nat -> Int.
```

But we also want to identify equal integers so we add the following rule:

```
[m,n] Diff (S m) (S n) --> Diff m n.
```

So the only possible normal forms of type `Int` are

*   `Diff 0 0`, representing the integer 0,
*   `Diff (S n) 0` for some `n : Nat`, representing the integer n+1,
*   `Diff 0 (S n)` for some `n : Nat`, representing the integer -(n+1).

Define a function `abs` with the following signature:

```
def abs : Int -> Nat.
```

Such that `abs n` reduces to the absolute value of `n`.

The constructor `Diff` is in fact a partial function because it reduces on some inputs (when both arguments are successors) but not all inputs.

However, irreducible terms of the form `Diff m n` are not considered bad terms but constructed values.

A constructor like `Diff` on which rewrite rules are added is called a **smart constructor**.

Please note that the notions of constructor, value and smart constructor have no meaning for Dedukti, they are just interpretations of the roles that are played by some symbols and normal forms.

Vectors and Dependent Types
---------------------------

Some functions on lists can be partially specified by their action on the length of the list; this can be reflected at the type level by making the type of lists dependent on the length of the list. Lists depending on their length are usually called vectors.

```
Vector : Nat -> Type.

vector_nil : Vector 0.
vector_cons : n : Nat -> A -> Vector n -> Vector (S n).
```

The type `Vector n` is the type of vectors of length `n`. A vector can be built like a list except that an extra argument has to be passed to `vector_cons` specifying the length of the tail of the vector.

Vectors allow us to write safe versions of the destructors `head` and `tail`:

```
def vector_head : n : Nat -> Vector (S n) -> A.
def vector_tail : n : Nat -> Vector (S n) -> Vector n.
[ n : Nat, a : A, l : Vector n ]
    vector_head n (vector_cons n a l) --> a.
[ n : Nat, a : A, l : Vector n ]
    vector_tail n (vector_cons n a l) --> l.
```

Define a function `vector_append` with the following signature:

```
def vector_append : m : Nat -> n : Nat -> Vector m -> Vector n -> Vector (plus m n).
```

computing vector concatenation.

Non-linear Rewriting
--------------------

### Partial Equality

If we want to define equality on lists or vectors, we need a way to compare elements but we don't have any constructor of type `A` yet.

However, we can partially define equality over `A` in the positive case:

```
def A_eq : A -> A -> Bool.
[a] A_eq a a --> True.
```

This rule is said to be **non-linear** because the free variable `a` appears twice in the left-hand side pattern. By default, Dedukti rejects non-linear rewrite rules; the option `-nl` can be passed to `dkcheck` to allow them.

Define the equalities over lists and vectors using `A_eq` and test them.

### Extending a decidable Equality modulo Conversion

We can also extend the equality over natural numbers by a similar non-linear rule:

```
[ n : Nat ] equal n n --> True.
```

With this extra rule, we will be able to consider that convertible terms of type `Nat` are equal even if they are not of the form `0` or `S n`; for example, `equal (pred 0) (pred 0)` and `equal (plus n n) (plus n n)` for an abstract `n` will both reduce to `True`.

Extend equalities over lists and vectors by a non-linear rule.

Symmetric Definitions
----------------------

### A Theorem about Vectors

Using Curry-De Brujn-Howard isomorphism, we can see Dedukti types as propositions (in First-Order Minimal Logic) and terms as proofs.

For example, the theorem stating that `0` is right-neutral for addition can be stated and proved in Dedukti like this:

```
def 0_right_neutral : n : Nat ->
                      Istrue (equal n (plus n 0)).
[] 0_right_neutral 0 --> tt
[n] 0_right_neutral (S n) --> 0_right_neutral n.
```

However, we can not even state a theorem for right-neutrality of `vector_nil` for `vector_assoc`; if we try like this:

```
def vector_nil_right_neutral : n : Nat ->
                               l : List n ->
                               Istrue (vector_equal n
                                        l
                                        (vector_assoc n 0 l vector_nil)).
```

dkcheck complains because `l` and `vector_assoc n 0 l vector_nil` do not have convertible types: `l` has type `Vector n` while `vector_assoc n 0 l vector_nil` has type `Vector (plus n 0)`.

We know that `n` and `plus n 0` are provably equal but for an abstract `n`, they are not convertible.

However, even for an abstract `n`, `n` and `plus 0 n` are convertible and so are `l` and `vector_assoc n 0 vector_nil l`.

This asymmetry comes from an asymmetry in the definition of `plus` (which replicates on `vector_assoc`).

In most type systems, we have to choose between a left and a right definition of Peano addition and then prove that the other possibilty is equivalent. In Dedukti we have a third option, a symmetric definition of `plus`:

```
def plus : Nat -> Nat -> Nat.
[n] plus 0 n --> n
[m] plus m 0 --> m
[m,n] plus m (S n) --> S (plus m n)
[m,n] plus (S m) n --> S (plus m n).
```

State and prove the `vector_nil_right_neutral` theorem.

Redefine `vector_assoc` symmetrically.

### Confluence

We have seen that both non-exhaustive (partial functions) and redundant pattern-matching are allowed and useful in Dedukti.

However, there are two requirements on the rewrite-system that are not checked by Dedukti but needed for a decidable type-checking:

*   strong normalization: there should be no infinite sequence of rewriting and β-reduction
*   confluence: each term should have only one normal form.

The higher-order confluence prover [CSIho](http://cl-informatik.uibk.ac.at/software/csi/ho/) can be used for automatic checking the confluence of Dedukti rewrite-systems using the -cc option (followed by the path to CSIho) of `dkcheck`.

Correct by Confluence
---------------------

In the previous parts, we have seen extensions of rewrite-systems defining `equal` and `plus` in order to enforce at the conversion level some properties of these functions: reflexivity and right-neutrality of `0`.

We can also add the associativity of addition:

```
[m,n,p] plus (plus m n) p --> plus m (plus n p).
```

Associativity of `plus` at the conversion level is useful for stating (or adding a rewrite-rule for) associativity of `vector_append`.

But even when we don't need a conversion test, it may be interesting to add a rewrite-rule. For example, monads can easily be defined in Dedukti and the monadic laws can be defined as rewrite-rules.

When we choose to use a rewrite-rule instead of proving a theorem, we ask all following terms, types, proofs and proposition to be considered modulo this rule so we don't have to say explicitly when we are using basic theorems like associativity of addition so our proofs are shorter and can be automatically found more easily. This technique is known as Deduction modulo.

An other advantage is that if we have an automatic way of checking that our systems are confluent and strongly normalizing, then we don't need to prove that `plus` is associative, has `0` as right-neutral element etc…, we have discharged this obligation to the confluence checker. This is what we mean by the expression **correct by confluence**.

All provably equal terms can not be made convertible since equality is in general not decidable. Explain while it is not possible to add the following rule stating that addition is commutative:

```
[m,n] plus n m --> plus m n.
```

Conclusion
----------

This tutorial was just an introduction to the Dedukti system and language illustrating some interesting features. Dedukti has successfully been used to encode computational calculi and a large variety of logical systems. Human-written Dedukti code, translators from several logical systems, automated theorem provers backends for the Dedukti language can be found [here](https://github.com/orgs/Deducteam/repositories).
