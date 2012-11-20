all:
	ocamlbuild -lib str dkparse.native
install:
	cp dkparse.native /usr/bin/dkparse
	cp lua/dedukti.lua /usr/share/lua/5.1/
profile:
	ocamlbuild -tag debug -tag profile dkparse.native
clean:
	ocamlbuild -clean
