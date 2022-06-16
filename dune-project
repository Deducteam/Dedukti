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
      (ocaml (>= 4.08))
      (cmdliner (>= 1.1.0))
      (tezt (and :with-test (>= 2.0)))
      menhir)
    (synopsis "An implementation of The Lambda-Pi Modulo Theory")
    (description "An implementation of The Lambda-Pi Modulo Theory"))

(package
        (name universo)
	(synopsis "A tool for Dedukti to play with universes")
	(description "A tool for Dedukti to play with universes")			
        (depends z3 (tezt (and :with-test (>= 2.0)))))
