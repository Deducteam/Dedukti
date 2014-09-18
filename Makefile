
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

OPTIONS = -cflags -inline,10 -ocamlc 'ocamlopt' -tag bin_annot -use-menhir # -tag debug -tag profile
MENHIR = -menhir "menhir --external-tokens Tokens"

all: dkcheck dktop dkdep dkrule doc

dkcheck:
	ocamlbuild -Is kernel,utils,parser,dkcheck $(OPTIONS) $(MENHIR) dkcheck.native

dktop:
	ocamlbuild -Is kernel,utils,parser,dktop $(OPTIONS) $(MENHIR) dktop.native

dkdep:
	ocamlbuild -Is kernel,utils,parser,dkdep $(OPTIONS) $(MENHIR) dkdep.native

dkrule:
	ocamlbuild -Is kernel,utils,parser,dkrule $(OPTIONS) $(MENHIR) dkrule.native

doc:
	ocamlbuild -Is kernel,utils,parser,dkcheck,dkrule dkcheck/dkcheck.docdir/index.html

install:
	install _build/dkcheck/dkcheck.native ${INSTALL_DIR}/dkcheck
	install _build/dktop/dktop.native ${INSTALL_DIR}/dktop
	install _build/dkdep/dkdep.native ${INSTALL_DIR}/dkdep
	install _build/dkrule/dkrule.native ${INSTALL_DIR}/dkrule

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

.PHONY: dkcheck dktop dkdep dkrule tests clean doc
