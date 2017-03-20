# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

all: skcheck sktop skdep skindent doc

skcheck:
	ocamlbuild -I skcheck skcheck.native

sktop:
	ocamlbuild -I sktop sktop.native

skdep:
	ocamlbuild -I skdep skdep.native

skindent:
	ocamlbuild -I skindent skindent.native

doc:
	ocamlbuild -Is kernel kernel/dedukti.docdir/index.html
	ocamlbuild -Is kernel kernel/dedukti.docdir/doc.tex
	ocamlbuild -Is kernel kernel/dedukti.docdir/dependencies.dot

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
