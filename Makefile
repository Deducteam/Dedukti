
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

MENHIR = -menhir "menhir --external-tokens Tokens"
SRC_DIRS = kernel,utils,parser

BINARIES=skcheck sktop skdep skrule skindent

all: lib $(BINARIES) doc

skcheck:
	ocamlbuild -Is $(SRC_DIRS),skcheck $(MENHIR) skcheck.native

sktop:
	ocamlbuild -Is $(SRC_DIRS),sktop $(MENHIR) sktop.native

skdep:
	ocamlbuild -Is $(SRC_DIRS),skdep $(MENHIR) skdep.native

skrule:
	ocamlbuild -Is $(SRC_DIRS),skrule $(MENHIR) skrule.native

skindent:
	ocamlbuild -Is $(SRC_DIRS),skindent $(MENHIR) skindent.native

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
	    rm "${INSTALL_DIR}/$$i" ; \
	done

clean:
	ocamlbuild -clean

tests: skdep skcheck
	@echo "run tests..."
	make -C tests/OK/ clean all
	@for i in tests/KO/*.sk ; do \
	    echo "on $$i...  " ; \
	    ./_skcheck/skcheck.native "$$i" 2>&1 | grep ERROR ; \
	done
	@echo "-----------------------"
	@echo "tests OK"

.PHONY: $(BINARIES) tests clean doc uninstall
