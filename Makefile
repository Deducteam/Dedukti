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

LOCALINCLUDES:= $(foreach dir, $(VPATH), -I $(dir))
MLINCLUDES:= $(LOCALINCLUDES) -I $(MYCAMLP4LIB)
BYTEFLAGS:= $(MLINCLUDES) -pp $(CAMLP4O) -g -rectypes -annot
OPTFLAGS:= $(MLINCLUDES) -pp $(CAMLP4O) -rectypes

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

all: $(BINARIES) t/Coq1univ.lua

byte : ./bin/coqine.byte$(EXE)
opt : ./bin/coqine$(EXE)

bin/coqine.byte$(EXE): src/coqine.cma src/main.ml
	$(OCAMLC) $(BYTEFLAGS) -o $@ unix.cma gramlib.cma $^

bin/coqine$(EXE): src/coqine.cmxa src/main.ml
	$(OCAMLOPT) $(OPTFLAGS) -o $@ unix.cmxa gramlib.cmxa $^

bin/parse$(EXE): src/coqine.cma src/parse.ml
	$(OCAMLC) $(BYTEFLAGS) -o $@ unix.cma gramlib.cma $^

bin/make_deps$(EXE): src/coqine.cmxa src/make_deps.ml
	$(OCAMLOPT) $(OPTFLAGS) -o $@ unix.cmxa gramlib.cmxa $^

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

%.cma: %.mllib | %.mllib.d
	$(OCAMLC) $(BYTEFLAGS) -a -o $@ $(filter-out %.mllib, $^)

%.cmxa: %.mllib | %.mllib.d
	$(OCAMLOPT) $(OPTFLAGS) -a -o $@ $(filter-out %.mllib, $^)

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

%.lua: %.dk
	dedukti $< > $@

%.dk: %.vo
	./bin/coqine -h $<

%.vo: %.v
	coqc $<

.PHONY: cleanconfig depclean clean distclean

cleanconfig:
	rm -f config/local.mk config/coqine_config.ml

depclean:
	rm -f lib/*.d src/*.d config/*.d

clean:
	rm -f $(BINARIES) lib/*.{cm*,o,a} src/*.{cm*,o,a} config/*.{cm*,o,a}

distclean: clean depclean cleanconfig

.PHONY: stats

stats:
	@echo STRUCTURE
	@cd src; wc names.ml term.ml declarations.ml environ.ml type_errors.ml
	@echo
	@echo COQINE
	@cd src; wc coqine.ml dkterm.ml
	@echo
	@echo INTERFACE
	@cd src; wc main.ml 
	@echo
	@echo TOTAL
	@cd src; wc *.ml | tail -1

-include $(DEPFILES)
