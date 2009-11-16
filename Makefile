-include config/local.mk

COQINESRC=.

MLDIRS=-I $(COQINESRC)/config -I $(COQINESRC)/lib -I $(COQINESRC)/src -I +camlp5
BYTEFLAGS=$(MLDIRS) -pp camlp5o -g
OPTFLAGS=$(MLDIRS) -pp camlp5o

BINARIES=./bin/coqine.byte$(EXE) ./bin/coqine$(EXE)

COQINECMA:= \
	config/coqine_config.cmo \
	$(addprefix lib/, \
		pp_control.cmo pp.cmo compat.cmo flags.cmo util.cmo \
		option.cmo hashcons.cmo system.cmo predicate.cmo rtree.cmo \
		envars.cmo ) \
	$(addprefix src/, \
		names.cmo univ.cmo validate.cmo esubst.cmo term.cmo \
		declarations.cmo environ.cmo closure.cmo reduction.cmo \
		type_errors.cmo modops.cmo inductive.cmo typeops.cmo indtypes.cmo \
		subtyping.cmo mod_checking.cmo safe_typing.cmo check.cmo ) \
	$(addprefix src/, euTerms.cmo coqine.cmo )

all: $(BINARIES)

byte : ./bin/coqine.byte$(EXE)
opt : ./bin/coqine$(EXE)

src/coqine.cma: $(COQINECMA)
	ocamlc $(BYTEFLAGS) -a -o $@ $(COQINECMA)

src/coqine.cmxa: $(COQINECMA:.cmo=.cmx)
	ocamlopt $(OPTFLAGS) -a -o $@ $(COQINECMA:.cmo=.cmx)

./bin/coqine.byte$(EXE): src/coqine.cma
	ocamlc $(BYTEFLAGS) -o $@ unix.cma gramlib.cma src/coqine.cma src/main.ml

./bin/coqine$(EXE): src/coqine.cmxa
	ocamlopt $(OPTFLAGS) -o $@ unix.cmxa gramlib.cmxa src/coqine.cmxa src/main.ml

stats:
	@echo STRUCTURE
	@cd src; wc names.ml term.ml declarations.ml environ.ml type_errors.ml
	@echo
	@echo COQINE
	@cd src; wc coqine.ml euTerms.ml
	@echo
	@echo INTERFACE
	@cd src; wc main.ml 
	@echo
	@echo TOTAL
	@cd src; wc *.ml | tail -1

.SUFFIXES:.ml .mli .cmi .cmo .cmx

.ml.cmo:
	$(OCAMLC) -c $(BYTEFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OPTFLAGS) $<

.mli.cmi:
	$(OCAMLC) -c $(BYTEFLAGS) $<

depend::
	ocamldep $(MLDIRS) -pp camlp5o config/*.{ml,mli} lib/*.{ml,mli} src/*.{ml,mli} > .depend

cleanconfig::
	rm -f config/local.mk config/coqine_config.ml

clean::
	rm -f $(BINARIES) lib/*.{cm*,o,a} src/*.{cm*,o,a} config/*.{cm*,o,a}

distclean: clean cleanconfig

-include .depend
