DKCHECK ?= ../../dkcheck.native
DKDEP ?=  ../../dkdep.native
DKOPTIONS = $(DKFLAGS) -nl -errors-in-snf -e

DKS = $(wildcard *.dk)
DKOS = $(DKS:.dk=.dko)

DKCHECK_=$(DKCHECK) $(DKOPTIONS)

.PHONY:	all clean

all: $(DKOS)

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
