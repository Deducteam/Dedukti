#put the path of your dkcheck binary
DKCHECK ?= dkcheck
#put the path of your dkdep binary
DKDEP ?= dkdep
DKOPTIONS = $(DKFLAGS) -e

DKS = $(wildcard *.dk)
DKOS = $(DKS:.dk=.dko)


ifdef DKTIME
DKCHECK_=time --format "TIME: %U" --output /dev/stdout $(DKCHECK) $(DKOPTIONS)
else
DKCHECK_=$(DKCHECK) $(DKOPTIONS)
endif


.PHONY:	all clean

all: empty easy medium hardest $(DKOS)

%.dko: %.dk .depend
	@echo "FILE: $<"
	$(DKCHECK_) $<

# standard .depend file generation
.depend: $(DKS)
	@echo "Producing .depend"
	@$(DKDEP) $^ -o $@

clean:
	@rm -f *.dko .depend tmp.dk

ifneq ($(MAKECMDGOALS), clean)
-include .depend
endif


empty  : solve_empty.dko
easy   : solve_easy.dko
medium : solve_medium.dko
hardest: solve_hardest.dko
