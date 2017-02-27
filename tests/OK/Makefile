SKCHECK = ../../skcheck.native
SKDEP = ../../skdep.native
DKCHECK = dkcheck
SKOPTIONS = -coc

SKS = $(wildcard *.sk)
DEPENDS = $(SKS:.sk=.depend)
DKOS = $(shell cut -d ':' -f 1 *.depend 2> /dev/null)
all: $(DKOS)

tests: all

builtins.dko: builtins.dk
	$(DKCHECK) -e -coc $<

%.dko:
	$(SKCHECK) $(SKOPTIONS) -e $<

%.depend: %.sk
	$(SKDEP) $< > $@

clean:
	rm -f *.dko *.depend

depend: $(DEPENDS)

-include $(DEPENDS)

.PHONY:	clean depend all
