
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

MENHIR = -menhir "menhir --external-tokens Tokens"
SRC_DIRS = kernel,utils,parser

BINARIES=skcheck skmeta sktop skdep skindent

all: lib $(BINARIES) doc

skmeta:
	ocamlbuild -Is $(SRC_DIRS),skmeta $(MENHIR) -lib unix skmeta.native

skcheck:
	ocamlbuild -Is $(SRC_DIRS),skcheck $(MENHIR) -lib unix skcheck.native

sktop:
	ocamlbuild -Is $(SRC_DIRS),sktop $(MENHIR) -lib unix sktop.native

skdep:
	ocamlbuild -Is $(SRC_DIRS),skdep $(MENHIR) -lib unix skdep.native

skindent:
	ocamlbuild -Is $(SRC_DIRS),skindent $(MENHIR) -lib unix skindent.native

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

tests: skdep skcheck
	@echo "run tests..."
	$(MAKE) -C tests/OK/ all
	@for i in tests/KO/*.sk ; do \
	    echo "on $$i...  " ; \
	    ./skcheck.native "$$i" 2>&1 | grep ERROR ; \
	done
	@echo "-----------------------"
	@echo "tests OK"

.PHONY: $(BINARIES) tests clean doc uninstall
