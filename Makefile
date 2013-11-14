
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin
OPTIONS = -cflags -inline,10

# DO NOT EDIT AFTER THIS LINE

all:
	ocamlbuild -build-dir _dkcheck $(OPTIONS) -ocamlc 'ocamlopt -rectypes' -cflags -rectypes -use-menhir -menhir "menhir --explain --external-tokens Types" dkcheck.native
	ocamlbuild -build-dir _dktop   $(OPTIONS) -ocamlc 'ocamlopt -rectypes' -cflags -rectypes -use-menhir -menhir "menhir --external-tokens Types" dktop.native
#	ocamlbuild -build-dir _dk2mmt  $(OPTIONS) -ocamlc 'ocamlopt -rectypes' -cflags -rectypes -use-menhir -menhir "menhir --external-tokens Types" dk2mmt.native

profile:
	ocamlbuild -tag profile -build-dir _dkcheck -ocamlc 'ocamlopt -rectypes' -cflags -rectypes -use-menhir -menhir "menhir --external-tokens Types" main.native

install:
	install _dkcheck/dkcheck.native ${INSTALL_DIR}/dkcheck
	install _dk2mmt/dk2mmt.native ${INSTALL_DIR}/dk2mmt
	install _dktop/dktop.native ${INSTALL_DIR}/dktop
	install _dkdep/dkdep.native ${INSTALL_DIR}/dkdep

clean:
	ocamlbuild -build-dir _dkcheck -clean
	ocamlbuild -build-dir _dktop -clean
	ocamlbuild -build-dir _dk2mmt -clean
	ocamlbuild -build-dir _dkdep -clean
