(lang dune 2.0)
(name dedukti)
(using menhir 2.0)

(generate_opam_files true)

(license CECILL-B)
(maintainers "dedukti-dev@inria.fr")
(authors "dedukti-dev@inria.fr")
(source (github Deducteam/Dedukti))

(package
    (name dedukti)
    (depends
      (ocaml (>= 4.06))
      cmdliner
      menhir)
    (synopsis "An implementation of The Lambda-Pi Modulo Theory")
    (description "An implementation of The Lambda-Pi Modulo Theory"))

(package
        (name universo)
        (depends z3))
