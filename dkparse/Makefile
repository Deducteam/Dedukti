LUA_LIB=/usr/share/lua/5.1/

all:
	ocamlbuild -libs str dkparse.native
install:
	cp dkparse.native /usr/bin/dkparse
	cp lua/dedukti.lua ${LUA_LIB}
clean:
	ocamlbuild -clean
