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

## Writing tests

Tests was originally written with `bash` scripts. We prefer now to use
[Tezt](https://tezos.gitlab.io/developer/tezt.html?highlight=tezt)
which is a test framework originally designed for
[Tezos](https://tezos.gitlab.io).

We invite and recommend developers to write new integration tests
using this framework.

### Useful tips with Tezt

- You can run `Tezt` tests with `dune exec tests/main.exe`

- You can list all the tests with `dune exec tests/main.exe -- --list`

- You can filter tests by tags. For example: `dune exec tests/main.exe -- check`.
  Tags can be negated or combined (see `dune exec tests/main.exe -- --help`)

- You can execute a single test using its name. For example:
  `dune exec tests/main.exe -- --title "check 'recursive.dk' succeeds"`

- When a modification changes the regression output, regression outputs can be reset with 
  `dune exec tests/main.exe -- --reset-regressions`.

- You can trigger the `Info` mode using `dune exec tests/main.exe -- -i`
  which will print the symbols which are type checked.

- You can trigger the `Verbose` mode using `dune exec tests/main.exe -- -v`
   which prints many debugging information.

- You can see the commands executed by `Tezt` using `dune exec tests/main.exe -- --commands`
