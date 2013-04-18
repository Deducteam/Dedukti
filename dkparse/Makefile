
# PLEASE EDIT THE FOLLOWING LINES TO FIT YOUR SYSTEM CONFIGURATION

INSTALL_DIR=/usr/bin
LUA_CMD=lua
LUAJIT_CMD=luajit-2.0.0-beta9
LUA_LIB=/usr/share/lua/5.1/

# DO NOT EDIT AFTER THIS LINE

all: dkc dkcheck dkchecknojit

dkc:
	ocamlbuild -libs str main.native

dkcheck:
	sed -i "2iLUA=${LUAJIT_CMD}" scripts/dkcheck

dkchecknojit:
	sed -i "2iLUA=${LUA_CMD}" scripts/dkchecknojit

install:
	install main.native ${INSTALL_DIR}/dkc
	install scripts/dkcheck ${INSTALL_DIR}/dkcheck
	install scripts/dkchecknojit ${INSTALL_DIR}/dkchecknojit
	install scripts/dkcompile ${INSTALL_DIR}/dkcompile
	install scripts/dk2mmt ${INSTALL_DIR}/dk2mmt
	install -d ${LUA_LIB}
	install --mode=644 lua/dedukti.lua ${LUA_LIB}

uninstall:
	rm -f ${INSTALL_DIR}/dkc
	rm -f ${INSTALL_DIR}/dkcheck
	rm -f ${INSTALL_DIR}/dkchecknojit
	rm -f ${INSTALL_DIR}/dkcompile
	rm -f ${INSTALL_DIR}/dk2mmt
	rm -f ${INSTALL_DIR}/dkparse
	rm -f ${INSTALL_DIR}/dedukti
	rm -f ${LUA_LIB}/lua/dedukti.lua

clean:
	ocamlbuild -clean
