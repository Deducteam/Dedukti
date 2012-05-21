CAMELIDE=../../camelide/src/camelide -v 2

.SUFFIXES: .dk .dko .o

.dk.dko:
	dedukti $<

.dko.o:
	../scripts/compile $<

include depend
