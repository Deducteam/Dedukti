OCAMLC=ocamlc
OCAMLOPT=ocamlopt
COQC=coqc -nois
EUROPA=dedukti
EURUN=dkrun
GHC=ghc
GHCI=ghci

COQSRC=..

MLDIRS=-I $(COQSRC) -I $(COQSRC)/config -I $(COQSRC)/lib -I $(COQSRC)/kernel -I +camlp5
BYTEFLAGS=$(MLDIRS) -g
OPTFLAGS=$(MLDIRS) 
GHCFLAGS=-x hs -XOverloadedStrings -fglasgow-exts

EXENAME=coqine

MCHECKERLOCAL :=\
  declarations.cmo environ.cmo \
  closure.cmo reduction.cmo \
  type_errors.cmo \
  modops.cmo \
  inductive.cmo typeops.cmo \
  indtypes.cmo subtyping.cmo mod_checking.cmo \
  safe_typing.cmo check.cmo \
  check_stat.cmo checker.cmo euTerms.cmo coqine.cmo main.cmo

MCHECKER:=\
  $(COQSRC)/config/coq_config.cmo \
  $(COQSRC)/lib/pp_control.cmo $(COQSRC)/lib/pp.cmo $(COQSRC)/lib/compat.cmo \
  $(COQSRC)/lib/flags.cmo $(COQSRC)/lib/util.cmo \
  $(COQSRC)/lib/option.cmo $(COQSRC)/lib/hashcons.cmo \
  $(COQSRC)/lib/system.cmo \
  $(COQSRC)/lib/predicate.cmo $(COQSRC)/lib/rtree.cmo \
  $(COQSRC)/kernel/names.cmo $(COQSRC)/kernel/univ.cmo \
  $(COQSRC)/kernel/envars.cmo \
  validate.cmo \
  $(COQSRC)/kernel/esubst.cmo term.cmo \
  $(MCHECKERLOCAL)

MCHECKEROPT=$(MCHECKER:.cmo=.cmx)

all: byte

byte : $(EXENAME)$(EXE)
opt : $(EXENAME).opt$(EXE)

$(EXENAME)$(EXE): main.cmo coqine.cmo
	ocamlc $(BYTEFLAGS) -o $@ unix.cma gramlib.cma $(MCHECKER)

$(EXENAME).opt$(EXE): main.cmx coqine.cmx
	ocamlopt $(OPTFLAGS) -o $@ unix.cmxa gramlib.cmxa $(MCHECKEROPT)

test: test.o 
	echo main | $(GHCI) $(GHCFLAGS) Coq1univ.dko test.dko

test.o: Coq1univ.o


.SUFFIXES:.ml .mli .cmi .cmo .cmx .v .vo .dk .dko .o .check

.ml.cmo:
	$(OCAMLC) -c $(BYTEFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OPTFLAGS) $<

.mli.cmi:
	$(OCAMLC) -c $(BYTEFLAGS) $<

.v.vo:
	$(COQC) $<

.vo.dk: $(EXENAME)$(EXE)
	./$(EXENAME)$(EXE) $< > $@

.dk.dko: 
	$(EUROPA) $<

.dko.o:
	$(GHC) $(GHCFLAGS) -c $<
	echo main | $(GHCI) $(GHCFLAGS) Coq1univ.dko $<

depend::
	ocamldep $(MLDIRS) -pp camlp5o config/*.{ml,mli} kernel/*.{ml,mli} lib/*.{ml,mli} *.{ml,mli} > .depend

clean::
	rm -f *.cm* *.o *.a *~ $(BINARIES)

debug: test.vo
	ledit ocamldebug -I .. -I ../kernel -I ../lib coqine test.vo

-include .depend
