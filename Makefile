
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

OPTIONS = -tag bin_annot -use-menhir # -tag debug -tag profile
MENHIR = -menhir "menhir --external-tokens Tokens"

all: dkcheck dktop dkdep dkrule doc

dkcheck:
	ocamlbuild -Is kernel,utils,parser,refiner,dkcheck $(OPTIONS) $(MENHIR) dkcheck.native

dktop:
	ocamlbuild -Is kernel,utils,parser,refiner,dktop $(OPTIONS) $(MENHIR) dktop.native

dkdep:
	ocamlbuild -Is kernel,utils,parser,refiner,dkdep $(OPTIONS) $(MENHIR) dkdep.native

dkrule:
	ocamlbuild -Is kernel,utils,parser,refiner,dkrule $(OPTIONS) $(MENHIR) dkrule.native

doc:
	ocamlbuild -Is kernel,utils,parser,dkcheck,dkrule,refiner dkcheck/dkcheck.docdir/index.html

BINARIES=dkcheck dktop dkdep dkrule

install: all
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
