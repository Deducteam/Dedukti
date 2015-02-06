SKCHECK = ../../skcheck.native
SKDEP = ../../skdep.native
SKOPTIONS = -coc

builtins.dko: builtins.dk
	dkcheck -e -coc $<

%.dko:
	$(SKCHECK) $(SKOPTIONS) -e $<

%.depend: %.sk
	$(SKDEP) $< > $@

clean:
	rm -f *.dko *.depend

SKS = $(wildcard *.sk)
DEPENDS = $(SKS:.sk=.depend)
DKOS = $(shell cat *.depend | cut -d ':' -f 1)
all: $(DKOS)

-include $(DEPENDS)

.PHONY:	clean all
