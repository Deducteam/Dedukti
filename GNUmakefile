# Current version number of Dedukti.
VERSION = devel

# Compile with "make Q=" to display the commands that are run.
Q = @

all: kernel parser commands META

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

#### Compilation of the parser library #######################################

PARSER_MLI := $(wildcard parser/*.mli)
PARSER_ML  := $(PARSER_MLI:.mli=.ml)
PARSER_GEN := parser/parser.mly parser/lexer.mll

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
commands: skcheck.native skdep.native skindent.native sktop.native

skcheck.native: kernel parser $(wildcard skcheck/*.ml skcheck/*.mli)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind skcheck/skcheck.native

skdep.native: kernel parser $(wildcard skdep/*.ml skdep/*.mli)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind skdep/skdep.native

skindent.native: kernel parser $(wildcard skindent/*.ml skindent/*.mli)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind skindent/skindent.native

sktop.native: kernel parser $(wildcard sktop/*.ml sktop/*.mli)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -use-ocamlfind sktop/sktop.native

#### Generation of the META file #############################################

META: GNUmakefile
	@echo "[GEN] $@"
	@echo 'name = "dedukti"'                                             > META
	@echo 'version = "$(VERSION)"'                                      >> META
	@echo 'description = "Dedukti library - λΠ-calculus modulo theory"' >> META
	@echo 'requires = "unix"'                                           >> META
	@echo 'archive(byte) = "kernel.cma, parser.cma"'                    >> META
	@echo 'archive(native) = "kernel.cmxa, parser.cmxa"'                >> META
	@echo                                                               >> META
	@echo 'package "kernel" ('                                          >> META
	@echo '  version = "$(VERSION)"'                                    >> META
	@echo '  description = "Dedukti kernel"'                            >> META
	@echo '  requires = "unix"'                                         >> META
	@echo '  archive(byte) = "kernel.cma"'                              >> META
	@echo '  archive(native) = "kernel.cmxa"'                           >> META
	@echo ')'                                                           >> META
	@echo                                                               >> META
	@echo 'package "parser" ('                                          >> META
	@echo '  version = "$(VERSION)"'                                    >> META
	@echo '  description = "Dedukti parser"'                            >> META
	@echo '  requires = "unix, dedukti.kernel"'                         >> META
	@echo '  archive(byte) = "parser.cma"'                              >> META
	@echo '  archive(native) = "parser.cmxa"'                           >> META
	@echo ')'                                                           >> META

#### Installation targets ####################################################

BINDIR = $(dir $(shell which ocaml))

.PHONY: uninstall
uninstall:
	@ocamlfind remove dedukti
	@rm -f $(BINDIR)/skcheck
	@rm -f $(BINDIR)/skdep
	@rm -f $(BINDIR)/skindent
	@rm -f $(BINDIR)/sktop

.PHONY: install
install: uninstall all
	@ocamlfind install dedukti META \
		$(wildcard _build/kernel/*.mli) $(wildcard _build/kernel/*.cmi) \
		$(wildcard _build/kernel/*.cmo) $(wildcard _build/kernel/*.cmx) \
		$(wildcard _build/kernel/*.o) _build/kernel/kernel.cma \
		_build/kernel/kernel.cmxa _build/kernel/kernel.a \
		$(wildcard _build/parser/*.mli) $(wildcard _build/parser/*.cmi) \
		$(wildcard _build/parser/*.cmo) $(wildcard _build/parser/*.cmx) \
		$(wildcard _build/parser/*.o) _build/parser/parser.cma \
		_build/parser/parser.cmxa _build/parser/parser.a
	install -m 755 -d $(BINDIR)
	install -m 755 -p skcheck.native  $(BINDIR)/skcheck
	install -m 755 -p skdep.native    $(BINDIR)/skdep
	install -m 755 -p skindent.native $(BINDIR)/skindent
	install -m 755 -p sktop.native    $(BINDIR)/sktop

#### Test targets ############################################################

.PHONY: tests
tests: skcheck.native tests/tests.sh
	tests/tests.sh

#### Cleaning targets ########################################################

clean:
	$(Q)ocamlbuild -quiet -clean

distclean: clean
	$(Q)find -name "*~" -exec rm {} \;
	$(Q)rm -f kernel/version.ml
	$(Q)rm -f META
