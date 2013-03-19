LUA_LIB=/usr/share/lua/5.1/

all:
	ocamlbuild -libs str dkparse.native
install:
	install dkparse.native /usr/bin/dkparse
	install -d ${LUA_LIB}
	install --mode=644 lua/dedukti.lua ${LUA_LIB}
clean:
	ocamlbuild -clean
