 LUA=luajit-2.0.0-beta9
#LUA=lua
DK=dkparse

all:

-include depend

%.lua:%.dk Coq1univ.lua
	${DK} -q $< | ${LUA} -
#	${DK} -q -c -o $@ $<

Coq1univ.lua:../t/Coq1univ.dk
	${DK} -q -c -o $@ $<

%.dk:
	exit 1
		        
clean:
	rm -f *.lua


