# Contributing to Dedukti

## Format the OCaml code

The code is formatted using
[ocamlformat](https://github.com/ocaml-ppx/ocamlformat/).
`ocamlformat` can be integrated with common editors as explained in
the documentation.

In any case, to ensure files are formatted properly, you can execute
`make fmt`. If you see a diff, it means files are not properly
formatted. You can apply `ocamlformat` using the command

```bash
dune build @fmt --auto-promote
```

Such reformatting impacts the whole codebase and can be troublesome
when using `git blame`. For that reasons, commits we do not want to
show using `git blame` can be ignored. Those commits are written in
the file `.git-blame-ignore-revs`. To effectively ignore those commits
you have to run the following command:

```bash
git config --local blame.ignoreRevsFile .git-blame-ignore-revs
```

### Change the formatting

If there is a consensus between the developpers, the formatting can be
changed. Do not hesitate to make suggestions. In that case, do not
forget to edit the `.git-blame-ignore-revs` file.


