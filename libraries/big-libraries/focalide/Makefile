#put the path of your dkcheck binary
DKCHECK ?= dkcheck
#put the path of your dkdep binary
DKDEP ?= dkdep
DKOPTIONS =  --errors-in-snf

DKS = $(wildcard *.dk)
DEPENDS = $(DKS:.dk=.depend)
DKOS = $(shell cat *.depend | cut -d ':' -f 1)

.PHONY:	all clean

all: $(DKOS)

clean:
	rm -f *.dko *.depend

%.dko:
	@echo "FILE: $<"
	#time --format "TIME: %U" --output /dev/stdout $(DKCHECK) $(DKOPTIONS) -e $<
	$(DKCHECK) $(DKOPTIONS) -e $<
%.depend: %.dk
	$(DKDEP) $< > $@

-include $(DEPENDS)
