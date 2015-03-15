
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

MENHIR = -menhir "menhir --external-tokens Tokens"
SRC_DIRS = kernel,utils,parser

all: lib dkcheck dktop dkdep dkrule dkindent doc

dkcheck:
	ocamlbuild -Is $(SRC_DIRS),dkcheck $(MENHIR) dkcheck.native

dktop:
	ocamlbuild -Is $(SRC_DIRS),dktop $(MENHIR) dktop.native

dkdep:
	ocamlbuild -Is $(SRC_DIRS),dkdep $(MENHIR) dkdep.native

dkrule:
	ocamlbuild -Is $(SRC_DIRS),dkrule $(MENHIR) dkrule.native

dkindent:
	ocamlbuild -Is $(SRC_DIRS),dkindent $(MENHIR) dkindent.native

doc:
	ocamlbuild -Is kernel kernel/dedukti.docdir/index.html

lib:
	ocamlbuild -Is kernel $(OPTIONS) dedukti.cmxa

BINARIES=dkcheck dktop dkdep dkrule

install:
	for i in $(BINARIES) ; do \
	    install "_build/$$i/$$i.native" "${INSTALL_DIR}/$$i" ; \
	done

uninstall:
	for i in $(BINARIES) ; do \
	    rm "${INSTALL_DIR}/$$i" ; \
	done

clean:
	ocamlbuild -clean

tests: dkcheck
	@echo "run tests..."
	@for i in tests/OK/*.dk ; do \
	    echo "on $$i...  " ; \
	    ./_dkcheck/dkcheck.native "$$i" 2>&1 | grep SUCCESS ; \
	done
	@for i in tests/KO/*.dk ; do \
	    echo "on $$i...  " ; \
	    ./_dkcheck/dkcheck.native "$$i" 2>&1 | grep ERROR ; \
	done
	@echo "-----------------------"
	@echo "tests OK"

.PHONY: dkcheck dktop dkdep dkrule dkindent tests clean doc uninstall
