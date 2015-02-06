
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

OPTIONS = -cflags -inline,10,-for-pack,Dedukti -ocamlc 'ocamlopt' -tag bin_annot -use-menhir # -tag debug -tag profile
MENHIR = -menhir "menhir --external-tokens Tokens"

all: dkcheck.native dktop.native dkdep.native dkrule.native _build/dkcheck/dkcheck.docdir/index.html

dkcheck:dkcheck.native
dktop:dktop.native
dkdep:dkdep.native
dkrule:dkrule.native
lib:_build/dedukti.cmxa
doc:_build/dkcheck/dkcheck.docdir/index.html

dkcheck.native:
	ocamlbuild -Is kernel,utils,parser,refiner,dkcheck $(OPTIONS) $(MENHIR) dkcheck.native

dktop.native:
	ocamlbuild -Is kernel,utils,parser,refiner,dktop $(OPTIONS) $(MENHIR) dktop.native

dkdep.native:
	ocamlbuild -Is kernel,utils,parser,refiner,dkdep $(OPTIONS) $(MENHIR) dkdep.native

dkrule.native:
	ocamlbuild -Is kernel,utils,parser,refiner,dkrule $(OPTIONS) $(MENHIR) dkrule.native

_build/dkcheck/dkcheck.docdir/index.html:
	ocamlbuild -Is kernel,utils,parser,dkcheck,dkrule,refiner dkcheck/dkcheck.docdir/index.html

_build/dedukti.cmxa:
	ocamlbuild -Is kernel,utils,parser $(OPTIONS) dedukti.cmxa

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

.PHONY: dkcheck dktop dkdep dkrule tests clean doc uninstall
