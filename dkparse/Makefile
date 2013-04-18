LUA_LIB=/usr/share/lua/5.1/

all:
	ocamlbuild -libs str main.native
install:
	install main.native /usr/bin/dkc
	install scripts/dedukti /usr/bin/dedukti
	install scripts/dkjit /usr/bin/dkjit
	install scripts/dk2mmt /usr/bin/dk2mmt
	install -d ${LUA_LIB}
	install --mode=644 lua/dedukti.lua ${LUA_LIB}
clean:
	ocamlbuild -clean
