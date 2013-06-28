
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

all: 
	ocamlbuild -use-menhir main.native

profile:
	ocamlbuild -use-menhir -tag profile main.native

install:
	install main.native ${INSTALL_DIR}/lpcheck

clean:
	ocamlbuild -clean
