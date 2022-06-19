# Interoperability in Dedukti

In this folder, I have made a small toy example to show how we can
implement logic transformations inside Dedukti. This toy example
starts from proof written in a simple logic called *sttfa*. This logic
is the onde used for the [Logipedia](www.logipedia.com) project.  The
purpose is to translate those proofs in the Coq logic. Or more
specifically, in a subpart of the Coq logic.

One of the goal is to show why this approach can scale in practice and
avoid the quadratic number of translations.

Our methodology relies on two tools:

- dkmeta: This tool allows to write convenient transformations in
  Dedukti.
  
- universo: This tool allows to play with universes in Dedukti. In
  practice, several interoperability problems boil down to a universe
  problem: Predicativity or dependent types are some examples.
  Moreover, "aligning" the universes is always a necessary step when
  we go from one theory to another because almost no theory share
  exactly the same hierarchy of universes (devil is in the details).

This repository is split into several directory:

- `theories`: Contains the two theories used in this
  example. `sttfa.dk` is the representation of `sttfa` in
  Dedukti. `cts.dk` is a generic representation of `cts` in Dedukti
  with two specifications: One for sttfa, and one for Coq.

- `example`: Contains one file `eq.dk` which needs to be filled. This
  file aims to contain a definition of the equality and the
  reflexivity of equality in sttfa. The expected solution can be found
  in `example/solution/eq.dk`. It also contains a file `nat.dk` that
  is here for the curious reader. It shows how inductive types can be
  encoded into Dedukti.

- `meta`: Is the directory used to transform proofs written in sttfa
  into the cts representation of sttfa. The input file will be the one
  in `example/eq.dk`.

- `universo`: Is the directory used by `universo` to transform proofs
  written in the cts representation of sttfa into the cts
  reprenstation of Coq (well a subpart of Coq).

# How to use

- Ensure you have Dedukti 2.7 installed. You can also run `make &&
  make install` at the root of the dedukti project.

- A first step is to fill `example/eq.dk`. If you don't want to fill
  it, consider running `make solution` first. Once it is done you can
  run `make meta` and `make universo`. The output for those two
  commands can be observed in `meta/output` and `universo/output`
  directories.

- Dependending on your curiosity, feel free to tweak the project. I
  can take any of your questions at `franth2@gmail.com`. Some remarks if you do so:

  - The makefile assumes everything is in `example/eq.dk`. You can
    create new files, but dependencies will have to be set manually.
    
  - We use the `dk prune` tool to prune the files. If you add more
    definitions, please consider modifying the file
    `meta/prune/config.dk`

  - More detailed on those tools can be found in [my PhD
    thesis](https://hal.archives-ouvertes.fr/tel-03224039/document)
  





