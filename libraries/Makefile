.PHONY:	all clean

ROOTDIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

DK = $(abspath $(ROOTDIR)/../dk.native)

DKCHECK=$(DK) check

DKDEP=$(DK) dep

all:
	$(MAKE) -C $(ROOTDIR)/theories DKDEP="$(DKDEP)" DKCHECK="$(DKCHECK)"
	$(MAKE) -C $(ROOTDIR)/paradoxes DKDEP="$(DKDEP)" DKCHECK="$(DKCHECK)"
	$(MAKE) -C $(ROOTDIR)/examples/sudoku DKDEP="$(DKDEP)" DKCHECK="$(DKCHECK)" medium
	$(MAKE) -C $(ROOTDIR)/examples/dpll DKDEP="$(DKDEP)" DKCHECK="$(DKCHECK)"

clean:
	$(MAKE) -C $(ROOTDIR)/theories         clean
	$(MAKE) -C $(ROOTDIR)/paradoxes        clean
	$(MAKE) -C $(ROOTDIR)/examples/sudoku  clean
	$(MAKE) -C $(ROOTDIR)/examples/dpll    clean
