# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/users/lsv/genestier/bin

# DO NOT EDIT AFTER THIS LINE

MENHIR = -menhir "menhir --external-tokens Tokens"

all: dktest lib doc

dktest :
	ocamlbuild -I $@ $(MENHIR) $@.native

doc:
	ocamlbuild -Is kernel kernel/dedukti.docdir/index.html
	ocamlbuild -Is kernel kernel/dedukti.docdir/doc.tex
	ocamlbuild -Is kernel kernel/dedukti.docdir/dependencies.dot

lib:
	ocamlbuild -Is kernel,utils,parser $(OPTIONS) dedukti.cmxa

BINARIES=dktest

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

tests: dktest
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

.PHONY: dkcheck dktest dktop dkdep dkindent lib tests clean doc install uninstall
