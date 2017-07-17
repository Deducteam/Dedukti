# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

MENHIR = -menhir "menhir --external-tokens Tokens"

PACKAGE = ocamlgraph


all: dkcheck dktop dkdep dkindent lib doc universo

dkcheck:
	ocamlbuild -I $@ $(MENHIR) $@.native

dktop:
	ocamlbuild -I $@ $(MENHIR) $@.native

dkdep:
	ocamlbuild -I $@ $(MENHIR) $@.native

dkindent:
	ocamlbuild -I $@ $(MENHIR) $@.native

universo:
	ocamlbuild -Is universo,universo/unionfind,universo/shortestpath $(MENHIR) -package $(PACKAGE) universo.native

doc:
	ocamlbuild -Is kernel kernel/dedukti.docdir/index.html
	ocamlbuild -Is kernel kernel/dedukti.docdir/doc.tex
	ocamlbuild -Is kernel kernel/dedukti.docdir/dependencies.dot

lib:
	ocamlbuild -Is kernel,utils,parser $(OPTIONS) dedukti.cmxa

BINARIES=dkcheck dktop dkdep dkindent

install:
	for i in $(BINARIES) ; do \
	    install "_build/$$i/$$i.native" "${INSTALL_DIR}/$$i" ; \
	done

uninstall:
	for i in $(BINARIES) ; do \
	    rm -f "${INSTALL_DIR}/$$i" ; \
	done

clean:
	ocamlbuild -clean

tests: dkcheck
	@echo "run tests..."
	@for i in tests/OK/*.dk ; do \
	    echo "on $$i...  " ; \
	    ./dkcheck.native "$$i" || exit 1; \
	done
	@for i in tests/KO/*.dk ; do \
	    echo "on $$i...  " ; \
	    ./dkcheck.native "$$i" 2>&1 | grep ERROR ; \
	done
	@echo "-----------------------"
	@echo "tests OK"

.PHONY: dkcheck dktop dkdep dkindent universo tests clean doc uninstall
