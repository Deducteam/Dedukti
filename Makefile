
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

OPTIONS = -cflags -inline,10 -ocamlc 'ocamlopt -rectypes' -cflags -rectypes -tag bin_annot -use-menhir # -tag profile
MENHIR = -menhir "menhir --external-tokens Types"

all: dkcheck dktop dkdep dkrule

dkcheck:
	ocamlbuild -Is kernel,utils,parser,dkcheck -build-dir _dkcheck $(OPTIONS) $(MENHIR) dkcheck/dkcheck.native

dktop:
	ocamlbuild -Is kernel,utils,parser,dktop -build-dir _dktop $(OPTIONS) $(MENHIR) dktop/dktop.native

dkdep:
	ocamlbuild -Is kernel,utils,parser,dkdep -build-dir _dkdep $(OPTIONS) $(MENHIR) dkdep.native

dkrule:
	ocamlbuild -Is kernel,utils,parser,dkrule -build-dir _dkrule $(OPTIONS) $(MENHIR) dkrule.native

doc:
	ocamlbuild -build-dir _dkcheck dkcheck.docdir/index.html

install:
	install _dkcheck/dkcheck.native ${INSTALL_DIR}/dkcheck
	install _dktop/dktop.native ${INSTALL_DIR}/dktop
	install _dkdep/dkdep.native ${INSTALL_DIR}/dkdep
	install _dkdep/dkrule.native ${INSTALL_DIR}/dkrule

clean:
	ocamlbuild -build-dir _dkcheck -clean
	ocamlbuild -build-dir _dktop -clean
	ocamlbuild -build-dir _dkdep -clean
	ocamlbuild -build-dir _dkrule -clean

.PHONY: dkcheck dktop dkdep dkrule
