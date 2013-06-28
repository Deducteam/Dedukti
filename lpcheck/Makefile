
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin

# DO NOT EDIT AFTER THIS LINE

all: 
	#ocamlbuild -libs str main.native
	ocamlbuild main.native

profile:
	ocamlbuild -tag profile main.native

install:
	install main.native ${INSTALL_DIR}/lpcheck

clean:
	ocamlbuild -clean
