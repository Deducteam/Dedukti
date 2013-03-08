 LUA=luajit-2.0.0-beta9
#LUA=lua
DK=dkparse

-include depend

%.lua:%.dk Coq1univ.lua
	${DK} $< | ${LUA} -
	${DK} -c -o $@ $<

Coq1univ.lua:../t/Coq1univ.dk
	${DK} -c -o $@ $<

%.dk:
	exit 1
		        
clean:
	rm -f *.lua
