# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

MENHIR = -menhir "menhir --external-tokens Tokens"

all: skcheck sktop skdep skindent doc

skcheck:
	ocamlbuild -I $@ $(MENHIR) $@.native

sktop:
	ocamlbuild -I $@ $(MENHIR) $@.native

skdep:
	ocamlbuild -I $@ $(MENHIR) $@.native

skindent:
	ocamlbuild -I $@ $(MENHIR) $@.native

doc:
	ocamlbuild -Is kernel kernel/dedukti.docdir/index.html
	ocamlbuild -Is kernel kernel/dedukti.docdir/doc.tex
	ocamlbuild -Is kernel kernel/dedukti.docdir/dependencies.dot

BINARIES=skcheck sktop skdep skindent
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

.PHONY: skcheck sktop skdep skindent tests clean doc install uninstall
