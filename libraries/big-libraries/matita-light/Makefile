DKS=$(wildcard *.dk)

# DKOS need not have the same name that the corresponding Dedukti
# files (modulogic.dk becomes zen.dko) but for each file f.dk, dkdep
# f.dk outputs the name of the produced dko file before the ':'
# character.
DKOS=$(shell cut -d ':' -f 1 .depend)

DKDEP ?= dkdep
DKCHECK ?= dkcheck

all: $(DKOS)

%.dko:
	$(DKCHECK) -e  $<

clean:
	rm -f *.dko .depend

depend: .depend
.depend:
	$(DKDEP) *.dk > ./.depend

-include .depend
