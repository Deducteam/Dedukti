
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

MENHIR = -menhir "menhir --external-tokens Tokens"
SRC_DIRS = kernel,utils,parser

BINARIES=dkcheck dktop dkdep dkindent ediloh

all: lib $(BINARIES) doc

ediloh:
	ocamlbuild -Is $(SRC_DIRS),ediloh $(MENHIR) -lib unix ediloh.native

dkcheck:
	ocamlbuild -Is $(SRC_DIRS),dkcheck $(MENHIR) -lib unix dkcheck.native

dktop:
	ocamlbuild -Is $(SRC_DIRS),dktop $(MENHIR) -lib unix dktop.native

dkdep:
	ocamlbuild -Is $(SRC_DIRS),dkdep $(MENHIR) -lib unix dkdep.native

dkindent:
	ocamlbuild -Is $(SRC_DIRS),dkindent $(MENHIR) -lib unix dkindent.native

doc:
	ocamlbuild -Is kernel kernel/dedukti.docdir/index.html

lib:
	ocamlbuild -Is kernel $(OPTIONS) dedukti.cmxa

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


.PHONY: $(BINARIES) tests clean doc uninstall
