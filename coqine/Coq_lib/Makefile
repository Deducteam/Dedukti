CAMELIDE=../../camelide/src/camelide -v 2

.SUFFIXES: .dk .dko .o .lua

.dk.lua:
	dedukti $< > $@
	cat Coq1univ.lua `echo $^ | sed 's/^.*[.]dk//'` | lua -l dedukti -

include depend
