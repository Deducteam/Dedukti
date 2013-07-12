
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

all: 
	ocamlbuild -ocamlc 'ocamlc.opt -rectypes' -cflags -rectypes -use-menhir main.native

# FIXME

profile:
	ocamlbuild -use-menhir -tag profile main.native

debug:
	ocamlbuild -use-menhir -tag debug main.byte

install:
	install main.native ${INSTALL_DIR}/lpcheck

clean:
	ocamlbuild -clean
