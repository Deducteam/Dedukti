-include config/local.mk

ifndef COQINE_CONFIGURED
$(error Please run ./configure first)
endif

# We use a vpath so that source files may be found automatically
# without having to explicitly tell make in what folders to find them.
# This makes senses because it amounts to reflecting the search paths
# of ocaml given via -I directives into make.
VPATH = config lib src
GPATH = $(VPATH)

MLINCLUDES:= -I config -I lib -I src -I +camlp5
BYTEFLAGS:= $(MLINCLUDES) -pp camlp5o -g
OPTFLAGS:= $(MLINCLUDES) -pp camlp5o

BINARIES:= bin/coqine.byte$(EXE) bin/coqine$(EXE)

MLFILES:= $(wildcard config/*.ml lib/*.ml src/*.ml)
MLLIBFILES:= $(wildcard config/*.mllib lib/*.mllib src/*.mllib)
ML4FILES:= $(wildcard src/*.ml4)
INTFFILES:= $(wildcard config/*.mli lib/*.mli src/*.mli)
IMPLFILES:= $(MLFILES) $(ML4FILES)
CMOFILES:= $(addsuffix .cmo, $(basename $(IMPLFILES)))
CMXOFILES:= $(addsuffix .cmxo, $(basename $(IMPLFILES)))

DEPFILES:= $(addsuffix .d, $(IMPLFILES) $(INTFFILES) $(MLLIBFILES)) \
           $(addsuffix .ml.d, $(ML4FILES))
.SECONDARY: $(DEPFILES)

CAMLP4DEPS=sed -n -e 's@^(\*.*camlp4deps: "\(.*\)".*\*)@\1@p'
CAMLP4USE=sed -n -e 's@^(\*.*camlp4use: "\(.*\)".*\*)@\1@p'

DEPFLAGS:= $(MLINCLUDES) -pp $(CAMLP4O)

all: $(BINARIES)

byte : ./bin/coqine.byte$(EXE)
opt : ./bin/coqine$(EXE)

bin/coqine.byte$(EXE): src/coqine.cma
	$(OCAMLC) $(BYTEFLAGS) -o $@ unix.cma gramlib.cma $<

bin/coqine$(EXE): src/coqine.cmxa
	$(OCAMLOPT) $(OPTFLAGS) -o $@ unix.cmxa gramlib.cmxa $<

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

%.cmo: %.ml | %.ml.d
	$(OCAMLC) $(BYTEFLAGS) -c $<

%.cmx: %.ml | %.ml.d
	$(OCAMLOPT) $(OPTFLAGS) -c $<

%.cmi: %.mli | %.mli.d
	$(OCAMLC) $(BYTEFLAGS) -c $<

ifdef NO_RECALC_DEPS
  D_DEPEND_BEFORE_SRC:= |
  D_DEPEND_AFTER_SRC:=
else
  D_DEPEND_BEFORE_SRC:=
  D_DEPEND_AFTER_SRC:= |
endif

%.ml.d: $(D_DEPEND_BEFORE_SRC) %.ml $(D_DEPEND_AFTER_SRC)
	$(OCAMLDEP) $(DEPFLAGS) "$<" | sed '' > "$@"

%.mli.d: $(D_DEPEND_BEFORE_SRC) %.mli $(D_DEPEND_AFTER_SRC)
	$(OCAMLDEP) $(DEPFLAGS) "$<" | sed '' > "$@"

# Note: this recipe only works when filenames start with a lower case
# letter, as is the convention for file names in ocaml.
%.mllib.d: $(D_DEPEND_BEFORE_SRC) %.mllib $(D_DEPEND_AFTER_SRC)
	cat < /dev/null > $@
	echo $*.cma: $(addsuffix .cmo, $(shell sed 's/./\l&/1' $<)) >> $@
	echo $*.cmxa: $(addsuffix .cmx, $(shell sed 's/./\l&/1' $<)) >> $@

%.cma: | %.mllib.d
	$(OCAMLC) $(BYTEFLAGS) -a -o $@ $^

%.cmxa: | %.mllib.d
	$(OCAMLOPT) $(OPTFLAGS) -a -o $@ $^

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
%.cmo: %.ml4 | %.ml4.ml.d %.ml4.d
	$(OCAMLC) $(BYTEFLAGS) -pp "$(CAMLP4O) `$(CAMLP4USE) $<` `$(CAMLP4DEPS) $<` $(CAMLP4COMPAT) -impl" -c -impl $<

%.cmx: %.ml4 | %.ml4.ml.d %.ml4.d
	$(OCAMLOPT) $(OPTFLAGS) -pp "$(CAMLP4O) `$(CAMLP4USE) $<` `$(CAMLP4DEPS) $<` $(CAMLP4COMPAT) -impl" -c -impl $<

.PHONY: cleanconfig depclean clean distclean

cleanconfig:
	rm -f config/local.mk config/coqine_config.ml

depclean:
	rm -f lib/*.d src/*.d config/*.d

clean:
	rm -f $(BINARIES) lib/*.{cm*,o,a} src/*.{cm*,o,a} config/*.{cm*,o,a}

distclean: clean depclean cleanconfig

-include $(DEPFILES)
