
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

OPTIONS = -cflags -inline,10 -ocamlc 'ocamlopt -rectypes' -cflags -rectypes \
	 -use-menhir -menhir "menhir --external-tokens Types" -tag bin_annot

all: dkcheck dktop dkdep

dkcheck:
	ocamlbuild -build-dir _dkcheck $(OPTIONS) dkcheck.native

dktop:
	ocamlbuild -build-dir _dktop $(OPTIONS) dktop.native

dkdep:
	ocamlbuild -build-dir _dkdep $(OPTIONS) dkdep.native

profile:
	ocamlbuild -tag profile -build-dir  $(OPTIONS) _dkcheck dkchech.native

doc:
	ocamlbuild -build-dir _dkcheck dkcheck.docdir/index.html

install:
	install _dkcheck/dkcheck.native ${INSTALL_DIR}/dkcheck
	install _dktop/dktop.native ${INSTALL_DIR}/dktop
	install _dkdep/dkdep.native ${INSTALL_DIR}/dkdep

clean:
	ocamlbuild -build-dir _dkcheck -clean
	ocamlbuild -build-dir _dktop -clean
	ocamlbuild -build-dir _dkdep -clean
