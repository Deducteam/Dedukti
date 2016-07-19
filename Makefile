
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

MENHIR = -menhir "menhir --external-tokens Tokens"
SRC_DIRS = kernel,utils,parser
PACKAGE = ocamlgraph

all: dkcheck dktop dkdep dkindent lib doc universo

dkcheck:
	ocamlbuild -Is $(SRC_DIRS),dkcheck $(MENHIR) -lib unix dkcheck.native

dktop:
	ocamlbuild -Is $(SRC_DIRS),dktop $(MENHIR) -lib unix dktop.native

dkdep:
	ocamlbuild -Is $(SRC_DIRS),dkdep $(MENHIR) -lib unix dkdep.native

dkindent:
	ocamlbuild -Is $(SRC_DIRS),dkindent $(MENHIR) -lib unix dkindent.native

universo:
	ocamlbuild -Is $(SRC_DIRS),universo,universo/unionfind,universo/shortestpath $(MENHIR) -lib unix -package $(PACKAGE) universo.native

doc:
	ocamlbuild -Is kernel kernel/dedukti.docdir/index.html

lib:
	ocamlbuild -Is kernel $(OPTIONS) dedukti.cmxa

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
	    ./dkcheck.native "$$i" 2>&1 | grep SUCCESS ; \
	done
	@for i in tests/KO/*.dk ; do \
	    echo "on $$i...  " ; \
	    ./dkcheck.native "$$i" 2>&1 | grep ERROR ; \
	done
	@echo "-----------------------"
	@echo "tests OK"

.PHONY: dkcheck dktop dkdep dkindent universo tests clean doc uninstall
