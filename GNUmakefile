# Current version number of Sukerujo (must be the same as the Dedukti kernel lib).
VERSION = master

# Compile with "make Q=" to display the commands that are run.
Q = @

all: parser commands META

#### Compilation of the parser library #######################################

PARSER_MLI := $(wildcard parser/*.mli)
PARSER_ML  := $(PARSER_MLI:.mli=.ml)
PARSER_GEN := parser/menhir_parser.mly parser/lexer.mll

.PHONY: parser
parser: _build/parser/parser.cma _build/parser/parser.cmxa

_build/parser/parser.cma: $(PARSER_MLI) $(PARSER_ML) $(PARSER_GEN)
	@echo "[BYT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind -package dedukti.kernel parser/parser.cma

_build/parser/parser.cmxa: $(PARSER_MLI) $(PARSER_ML) $(PARSER_GEN)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind -package dedukti.kernel parser/parser.cmxa

#### Compilation of the Sukerujo suite ########################################

.PHONY: commands

commands: skcheck.native skdep.native sktop.native

skcheck.native: parser commands/skcheck.ml
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind -package dedukti.kernel commands/skcheck.native

skdep.native: parser commands/skdep.ml
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind -package dedukti.kernel commands/skdep.native

sktop.native: parser commands/sktop.ml
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind -package dedukti.kernel commands/sktop.native

#### Generation of the META file #############################################

META: GNUmakefile
	@echo "[GEN] $@"
	@echo 'name = "sukerujo"'                                           > META
	@echo 'version = "$(VERSION)"'                                      >> META
	@echo 'description = "Sukerujo library - syntactic sugar for Dedukti"' >> META
	@echo 'requires = "unix"'                                           >> META
	@echo 'archive(byte) = "parser.cma"'                                >> META
	@echo 'archive(native) = "parser.cmxa"'                             >> META
	@echo                                                               >> META
	@echo 'package "parser" ('                                          >> META
	@echo '  version = "$(VERSION)"'                                    >> META
	@echo '  description = "Sukerujo parser"'                           >> META
	@echo '  requires = "unix, dedukti.kernel"'                         >> META
	@echo '  archive(byte) = "parser.cma"'                              >> META
	@echo '  archive(native) = "parser.cmxa"'                           >> META
	@echo ')'                                                           >> META

#### Installation targets ####################################################

BINDIR = $(dir $(shell which ocaml))

.PHONY: uninstall
uninstall:
	@ocamlfind remove sukerujo
	@rm -f $(BINDIR)/skcheck
	@rm -f $(BINDIR)/skdep
	@rm -f $(BINDIR)/sktop


.PHONY: install
install: uninstall all
	@ocamlfind install sukerujo META \
		_build/parser/parser.mli _build/parser/parser.cmi \
		$(wildcard _build/parser/*.cmx) $(wildcard _build/parser/*.o) \
	        _build/parser/parser.cma \
		_build/parser/parser.cmxa \
		_build/parser/parser.a
	install -m 755 -d $(BINDIR)
	install -m 755 -p skcheck.native  $(BINDIR)/skcheck
	install -m 755 -p skdep.native    $(BINDIR)/skdep
	install -m 755 -p sktop.native    $(BINDIR)/sktop


#### Test targets ############################################################

.PHONY: tests

tests: all tests/tests.sh
	@./tests/tests.sh

.PHONY: full_tests
full_tests: all tests/external_tests.sh
	@./tests/external_tests.sh


#### Cleaning targets ########################################################

clean:
	$(Q)ocamlbuild -quiet -clean

distclean: clean
	$(Q)find -name "*~" -exec rm {} \;
	$(Q)rm -f META
