DKCHECK = ../../dkcheck.native
DKDEP = ../../dkdep.native
DKOPTIONS = -coc

builtins.dko: builtins.dk
	dkcheck -e -coc $<

builtins.depend: builtins.dk
	dkdep $< > $@

%.dko:
	$(DKCHECK) $(DKOPTIONS) -e $<

%.depend: %.dk
	$(DKDEP) $< > $@

clean:
	rm -f *.dko *.depend

DKS = $(wildcard *.dk)
DEPENDS = $(DKS:.dk=.depend)
DKOS = $(shell cat *.depend | cut -d ':' -f 1)
all: $(DKOS)

-include $(DEPENDS)

.PHONY:	clean all
