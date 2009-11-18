-include config/local.mk

ifndef COQINE_CONFIGURED
$(error Please run ./configure first)
endif

COQINESRC=.

MLDIRS=-I $(COQINESRC)/config -I $(COQINESRC)/lib -I $(COQINESRC)/src -I +camlp5
BYTEFLAGS=$(MLDIRS) -pp camlp5o -g
OPTFLAGS=$(MLDIRS) -pp camlp5o

BINARIES=./bin/coqine.byte$(EXE) ./bin/coqine$(EXE)

MLFILES:= $(wildcard config/*.ml lib/*.ml src/*.ml)
ML4FILES:= $(wildcard src/*.ml4)
INTFFILES:= $(wildcard config/*.mli lib/*.mli src/*.mli)
IMPLFILES:= $(MLFILES) $(ML4FILES)
CMOFILES:= $(addsuffix .cmo, $(basename $(IMPLFILES)))
CMXOFILES:= $(addsuffix .cmxo, $(basename $(IMPLFILES)))

DEPFILES:=$(addsuffix .d, $(IMPLFILES) $(INTFFILES)) \
          $(addsuffix .ml.d, $(ML4FILES))
.SECONDARY: $(DEPFILES)

CAMLP4DEPS=sed -n -e 's@^(\*.*camlp4deps: "\(.*\)".*\*)@\1@p'
CAMLP4USE=sed -n -e 's@^(\*.*camlp4use: "\(.*\)".*\*)@\1@p'

DEPFLAGS=$(MLDIRS) -pp $(CAMLP4O)

all: $(BINARIES)

byte : ./bin/coqine.byte$(EXE)
opt : ./bin/coqine$(EXE)

bin/coqine.byte$(EXE): $(CMOFILES)
	ocamlc $(BYTEFLAGS) -o $@ unix.cma gramlib.cma $(CMOFILES)

bin/coqine$(EXE):
	ocamlopt $(OPTFLAGS) -o $@ unix.cmxa gramlib.cmxa $(CMOFILES)

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

%.cmo: %.ml4 | %.ml4.ml.d %.ml4.d
	$(OCAMLC) $(BYTEFLAGS) -pp "$(CAMLP4O) `$(CAMLP4USE) $<` `$(CAMLP4DEPS) $<` $(CAMLP4COMPAT) -impl" -c -impl $<

%.cmx: %.ml4 | %.ml4.ml.d %.ml4.d
	$(OCAMLOPT) $(OPTFLAGS) -pp "$(CAMLP4O) `$(CAMLP4USE) $<` `$(CAMLP4DEPS) $<` $(CAMLP4COMPAT) -impl" -c -impl $<

%.cmo: %.ml | %.ml.d
	$(OCAMLC) $(BYTEFLAGS) -c $<

%.cmx: %.ml | %.ml.d
	$(OCAMLOPT) $(OPTFLAGS) -c $<

%.cmi: %.mli | %.mli.d
	$(OCAMLC) $(BYTEFLAGS) -c $<

ifdef NO_RECALC_DEPS
  D_DEPEND_BEFORE_SRC:=|
  D_DEPEND_AFTER_SRC:=
else
  D_DEPEND_BEFORE_SRC:=
  D_DEPEND_AFTER_SRC:=|
endif

%.ml.d: $(D_DEPEND_BEFORE_SRC) %.ml $(D_DEPEND_AFTER_SRC)
	$(OCAMLDEP) $(DEPFLAGS) "$<" | sed '' > "$@"

%.mli.d: $(D_DEPEND_BEFORE_SRC) %.mli $(D_DEPEND_AFTER_SRC)
	$(OCAMLDEP) $(DEPFLAGS) "$<" | sed '' > "$@"

## Veerry nasty hack to keep ocamldep happy
%.ml: | %.ml4
	echo "let keep_ocamldep_happy Do_not_compile_me = assert false" > $@

%.ml4.d: $(D_DEPEND_BEFORE_SRC) %.ml4
	( printf "%s" '$*.cmo $*.cmx $*.ml4.ml.d $*.ml4-preprocessed: $(SEP)' && $(CAMLP4DEPS) "$<" ) > "$@"

%.ml4.ml.d: $(D_DEPEND_BEFORE_SRC) %.ml4 $(D_DEPEND_AFTER_SRC) %.ml4.d
	$(CAMLP4O) pr_o.cmo `$(CAMLP4USE) $<` `$(CAMLP4DEPS) $<` $(CAMLP4COMPAT) -impl $< -o $*.ml \
	  || ( RV=$$?; rm -f "$*.ml"; exit $${RV} )
	$(OCAMLDEP) $(DEPFLAGS) $*.ml | sed '' > "$@" || ( RV=$$?; rm -f "$@"; exit $${RV} )
	echo "let keep_ocamldep_happy Do_not_compile_me = assert false" > $*.ml

.PHONY: cleanconfig depclean clean distclean

cleanconfig:
	rm -f config/local.mk config/coqine_config.ml

depclean:
	rm -f lib/*.d src/*.d config/*.d

clean:
	rm -f $(BINARIES) lib/*.{cm*,o,a} src/*.{cm*,o,a} config/*.{cm*,o,a}

distclean: clean depclean cleanconfig

-include $(DEPFILES)
