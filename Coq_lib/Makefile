.SUFFIXES: .dk .dko .o

.dk.dko:
	dedukti $< +RTS -K200000000

.dko.o:
	../scripts/compile $<

include depend
