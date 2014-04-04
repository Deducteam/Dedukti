
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
