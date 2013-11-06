
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

all: 
	ocamlbuild -build-dir _dkcheck -ocamlc 'ocamlopt -rectypes' -cflags -rectypes -use-menhir -menhir "menhir --external-tokens Types" main.native
	ocamlbuild -build-dir _dk2mmt  -ocamlc 'ocamlopt -rectypes' -cflags -rectypes -use-menhir -menhir "menhir --external-tokens Types" dk2mmt.native

#profile:
#	ocamlbuild -use-menhir -tag profile main.native

#debug:
#	ocamlbuild -use-menhir -tag debug main.byte

install:
	install _dkcheck/main.native ${INSTALL_DIR}/dkcheck
	install _dk2mmt/dk2mmt.native ${INSTALL_DIR}/dk2mmt

clean:
	ocamlbuild -build-dir _dkcheck -clean
	ocamlbuild -build-dir _dk2mmt -clean
