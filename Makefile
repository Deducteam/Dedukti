
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

OPTIONS = -cflags -inline,10,-for-pack,Dedukti -ocamlc 'ocamlopt' -tag bin_annot -use-menhir # -tag debug -tag profile
MENHIR = -menhir "menhir --external-tokens Tokens"

all: skcheck.native sktop.native skdep.native skrule.native skindent.native _build/skcheck/skcheck.docdir/index.html

skcheck:skcheck.native
sktop:sktop.native
skdep:skdep.native
skrule:skrule.native
skindent:skindent.native
lib:_build/dedukti.cmxa
doc:_build/skcheck/skcheck.docdir/index.html

skcheck.native:
	ocamlbuild -Is kernel,utils,parser,refiner,skcheck $(OPTIONS) $(MENHIR) skcheck.native

sktop.native:
	ocamlbuild -Is kernel,utils,parser,refiner,sktop $(OPTIONS) $(MENHIR) sktop.native

skdep.native:
	ocamlbuild -Is kernel,utils,parser,refiner,skdep $(OPTIONS) $(MENHIR) skdep.native

skrule.native:
	ocamlbuild -Is kernel,utils,parser,refiner,skrule $(OPTIONS) $(MENHIR) skrule.native

skindent.native:
	ocamlbuild -Is kernel,utils,parser,skindent $(OPTIONS) $(MENHIR) skindent.native

_build/skcheck/skcheck.docdir/index.html:
	ocamlbuild -Is kernel,utils,parser,skcheck,skrule,refiner skcheck/skcheck.docdir/index.html

BINARIES=skcheck sktop skdep skrule skindent

_build/dedukti.cmxa:
	ocamlbuild -Is kernel,utils,parser $(OPTIONS) dedukti.cmxa

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

.PHONY: skcheck sktop skdep skrule skindent tests clean doc uninstall
