# Current version number of Dedukti.
VERSION = devel

# Compile with "make Q=" to display the commands that are run.
Q = @

all: kernel api parser commands META

#### Compilation of the kernel library #######################################

KERNEL_MLI := $(wildcard kernel/*.mli)
KERNEL_ML  := $(KERNEL_MLI:.mli=.ml)

.PHONY: kernel
kernel: _build/kernel/kernel.cma _build/kernel/kernel.cmxa

kernel/version.ml: GNUmakefile
	@echo "[GEN] $@ ($(VERSION))"
	$(Q)echo 'let version = "$(VERSION)"' > $@

_build/kernel/kernel.cma: $(KERNEL_MLI) $(KERNEL_ML)
	@echo "[BYT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind kernel/kernel.cma

_build/kernel/kernel.cmxa: $(KERNEL_MLI) $(KERNEL_ML)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind kernel/kernel.cmxa

#### Compilation of the API library #######################################

API_MLI := $(wildcard api/*.mli)
API_ML  := $(API_MLI:.mli=.ml)

.PHONY: api
api: kernel _build/api/api.cma _build/api/api.cmxa

_build/api/api.cma: $(API_MLI) $(API_ML)
	@echo "[BYT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind api/api.cma

_build/api/api.cmxa: $(API_MLI) $(API_ML)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind api/api.cmxa

#### Compilation of the parser library #######################################

PARSER_MLI := $(wildcard parser/*.mli)
PARSER_ML  := $(PARSER_MLI:.mli=.ml)
PARSER_GEN := parser/menhir_parser.mly parser/lexer.mll

.PHONY: parser
parser: kernel _build/parser/parser.cma _build/parser/parser.cmxa

_build/parser/parser.cma: $(PARSER_MLI) $(PARSER_ML) $(PARSER_GEN)
	@echo "[BYT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind parser/parser.cma

_build/parser/parser.cmxa: $(PARSER_MLI) $(PARSER_ML) $(PARSER_GEN)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind parser/parser.cmxa

#### Compilation of the dedukti suite ########################################

.PHONY: commands
commands: dkcheck.native dkdep.native dktop.native

dkcheck.native: kernel api parser commands/dkcheck.ml
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind commands/dkcheck.native

dkdep.native: kernel api parser commands/dkdep.ml
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind commands/dkdep.native

dktop.native: kernel api parser commands/dktop.ml
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind commands/dktop.native

#### Generation of the documentation #########################################

.PHONY: doc
doc: _build/kernel/kernel.docdir/index.html

_build/kernel/kernel.docdir/index.html: $(KERNEL_MLI) $(KERNEL_ML)
	@echo "[DOC] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind kernel/kernel.docdir/index.html

#### Generation of the META file #############################################

META: GNUmakefile
	@echo "[GEN] $@"
	@echo 'name = "dedukti"'                                             > META
	@echo 'version = "$(VERSION)"'                                      >> META
	@echo 'description = "Dedukti library - λΠ-calculus modulo theory"' >> META
	@echo 'requires = "unix"'                                           >> META
	@echo 'archive(byte) = "kernel.cma, api.cma, parser.cma"'           >> META
	@echo 'archive(native) = "kernel.cmxa, api.cmxa, parser.cmxa"'      >> META
	@echo                                                               >> META
	@echo 'package "kernel" ('                                          >> META
	@echo '  version = "$(VERSION)"'                                    >> META
	@echo '  description = "Dedukti kernel"'                            >> META
	@echo '  requires = "unix"'                                         >> META
	@echo '  archive(byte) = "kernel.cma"'                              >> META
	@echo '  archive(native) = "kernel.cmxa"'                           >> META
	@echo ')'                                                           >> META
	@echo                                                               >> META
	@echo 'package "api" ('                                             >> META
	@echo '  version = "$(VERSION)"'                                    >> META
	@echo '  description = "Dedukti API"'                               >> META
	@echo '  requires = "unix, dedukti.kernel"'                         >> META
	@echo '  archive(byte) = "api.cma"'                                 >> META
	@echo '  archive(native) = "api.cmxa"'                              >> META
	@echo ')'                                                           >> META
	@echo                                                               >> META
	@echo 'package "parser" ('                                          >> META
	@echo '  version = "$(VERSION)"'                                    >> META
	@echo '  description = "Dedukti parser"'                            >> META
	@echo '  requires = "unix, dedukti.kernel, dedukti.api"'            >> META
	@echo '  archive(byte) = "parser.cma"'                              >> META
	@echo '  archive(native) = "parser.cmxa"'                           >> META
	@echo ')'                                                           >> META

#### Installation targets ####################################################

BINDIR = $(dir $(shell which ocaml))

.PHONY: uninstall
uninstall:
	@ocamlfind remove dedukti
	@rm -f $(BINDIR)/dkcheck
	@rm -f $(BINDIR)/dkdep
	@rm -f $(BINDIR)/dktop

.PHONY: install
install: uninstall all
	@ocamlfind install dedukti META \
		$(wildcard _build/kernel/*.mli) $(wildcard _build/kernel/*.cmi) \
		$(wildcard _build/kernel/*.cmx) $(wildcard _build/kernel/*.o) \
		$(wildcard _build/api/*.mli) $(wildcard _build/api/*.cmi) \
		$(wildcard _build/api/*.cmx) $(wildcard _build/api/*.o) \
		_build/parser/parser.mli _build/parser/parser.cmi \
		$(wildcard _build/parser/*.cmx) $(wildcard _build/parser/*.o) \
		_build/kernel/kernel.cma  _build/api/api.cma  _build/parser/parser.cma \
		_build/kernel/kernel.cmxa _build/api/api.cmxa _build/parser/parser.cmxa \
		_build/kernel/kernel.a    _build/api/api.a    _build/parser/parser.a
	install -m 755 -d $(BINDIR)
	install -m 755 -p dkcheck.native  $(BINDIR)/dkcheck
	install -m 755 -p dkdep.native    $(BINDIR)/dkdep
	install -m 755 -p dktop.native    $(BINDIR)/dktop

#### Test targets ############################################################

.PHONY: tests
tests: all tests/tests.sh
	@./tests/tests.sh

#### Library tests ###########################################################

.PHONY: matita
matita: all
	@echo "## Compiling the Matita's arithmetic library ##"
	@cd tests/libraries && ./matita.sh

.PHONY: matita-light
matita-light: all
	@echo "## Compiling the Matita's arithmetic library (light) ##"
	@cd tests/libraries && ./matita-light.sh

.PHONY: plein_de_dks
plein_de_dks: all
	@echo "## Compiling “plein de dks” ##"
	@cd tests/libraries && ./plein_de_dks.sh

.PHONY: focalide
focalide: all
	@echo "## Compiling focalide library ##"
	@cd tests/libraries && ./focalide.sh

.PHONY: holide
holide: all
	@echo "## Compiling holide library ##"
	@cd tests/libraries && ./holide.sh

.PHONY: verine
verine: all
	@echo "## Compiling verine library ##"
	@cd tests/libraries && ./verine.sh

.PHONY: iprover
iprover: all
	@echo "## Compiling iProverModulo library ##"
	@cd tests/libraries && ./iprover.sh

.PHONY: dklib
dklib: all
	@echo "## Compiling the dklib library ##"
	@cd tests/libraries && ./dklib.sh

.PHONY: zenon_modulo
zenon_modulo: all
	@echo "## Compiling the zenon library ##"
	@cd tests/libraries && ./zenon_modulo.sh


.PHONY: light_tests
light_tests: all matita-light dklib plein_de_dks

.PHONY: full_tests
full_tests: light_tests iprover holide focalide verine zenon_modulo


#### Cleaning targets ########################################################
.PHONY: clean
clean:
	$(Q)ocamlbuild -quiet -clean

.PHONY: distclean
distclean: clean
	@cd tests/libraries && ./matita.sh clean
	@cd tests/libraries && ./matita-light.sh clean
	@cd tests/libraries && ./plein_de_dks.sh clean
	@cd tests/libraries && ./focalide.sh clean
	@cd tests/libraries && ./holide.sh clean
	@cd tests/libraries && ./verine.sh clean
	@cd tests/libraries && ./iprover.sh clean
	@cd tests/libraries && ./dklib.sh clean
	@cd tests/libraries && ./zenon_modulo.sh clean
	$(Q)find -name "*~" -exec rm {} \;
	$(Q)rm -f kernel/version.ml
	$(Q)rm -f META

.PHONY: fullclean
fullclean: distclean
	@cd tests/libraries && ./matita.sh fullclean
	@cd tests/libraries && ./matita-light.sh fullclean
	@cd tests/libraries && ./plein_de_dks.sh fullclean
	@cd tests/libraries && ./focalide.sh fullclean
	@cd tests/libraries && ./holide.sh fullclean
	@cd tests/libraries && ./verine.sh fullclean
	@cd tests/libraries && ./iprover.sh fullclean
	@cd tests/libraries && ./dklib.sh fullclean
	@cd tests/libraries && ./zenon_modulo.sh fullclean
