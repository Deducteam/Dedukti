# Current version number of Dedukti.
VERSION = devel

# Compile with "make Q=" to display the commands that are run.
Q = @

.PHONY: all
all: bin binaries

.PHONY: binaries
binaries: dkcheck.native dktop.native dkdep.native dkprune.native

%.native:
	@ln -fs _build/install/default/bin/$* $@

.PHONY: bin
bin: kernel/version.ml
	@dune build

.PHONY: doc
doc:
	@dune build @doc

.PHONY: clean
clean:
	@dune clean
	@rm -f *.native

.PHONY: install
install: all
	@dune install

.PHONY: uninstall
uninstall: all
	@dune uninstall

kernel/version.ml: GNUmakefile
	$(Q)echo 'let version = "$(VERSION)"' > $@


#### Test targets ############################################################

.PHONY: tests
tests: all tests/tests.sh
	@./tests/tests.sh

#### Library tests ###########################################################

TEST_LIBS=libraries

.PHONY: matita
matita: all
	@echo "## Compiling the Matita's arithmetic library ##"
	@cd $(TEST_LIBS) && ./matita.sh

.PHONY: matita-light
matita-light: all
	@echo "## Compiling the Matita's arithmetic library (light) ##"
	@cd $(TEST_LIBS) && ./matita-light.sh

.PHONY: plein_de_dks
plein_de_dks: all
	@echo "## Compiling “plein de dks” ##"
	@cd $(TEST_LIBS) && ./plein_de_dks.sh

.PHONY: focalide
focalide: all
	@echo "## Compiling focalide library ##"
	@cd $(TEST_LIBS) && ./focalide.sh

.PHONY: holide
holide: all
	@echo "## Compiling holide library ##"
	@cd $(TEST_LIBS) && ./holide.sh

.PHONY: dedukti-libraries
dedukti-libraries: all
	@echo "## Compiling the Dedukti Libraries folder ##"
	@cd $(TEST_LIBS) && ./dedukti-libraries.sh

.PHONY: verine
verine: all
	@echo "## Compiling verine library ##"
	@cd $(TEST_LIBS) && ./verine.sh

.PHONY: iprover
iprover: all
	@echo "## Compiling iProverModulo library ##"
	@cd $(TEST_LIBS) && ./iprover.sh

.PHONY: dklib
dklib: all
	@echo "## Compiling the dklib library ##"
	@cd $(TEST_LIBS) && ./dklib.sh

.PHONY: zenon_modulo
zenon_modulo: all
	@echo "## Compiling the zenon library ##"
	@cd $(TEST_LIBS) && ./zenon_modulo.sh


.PHONY: light_tests
light_tests: all matita-light dklib holide

.PHONY: full_tests
full_tests: light_tests iprover focalide dedukti-libraries verine # zenon_modulo

.PHONY: cleanlibs
cleanlibs:
	@cd $(TEST_LIBS) && ./matita.sh            clean
	@cd $(TEST_LIBS) && ./matita-light.sh      clean
	@cd $(TEST_LIBS) && ./plein_de_dks.sh      clean
	@cd $(TEST_LIBS) && ./focalide.sh          clean
	@cd $(TEST_LIBS) && ./holide.sh            clean
	@cd $(TEST_LIBS) && ./verine.sh            clean
	@cd $(TEST_LIBS) && ./iprover.sh           clean
	@cd $(TEST_LIBS) && ./dklib.sh             clean
	@cd $(TEST_LIBS) && ./zenon_modulo.sh      clean
	@cd $(TEST_LIBS) && ./dedukti-libraries.sh clean

.PHONY: fullcleanlibs
fullcleanlibs:
	@cd $(TEST_LIBS) && ./matita.sh            fullclean
	@cd $(TEST_LIBS) && ./matita-light.sh      fullclean
	@cd $(TEST_LIBS) && ./plein_de_dks.sh      fullclean
	@cd $(TEST_LIBS) && ./focalide.sh          fullclean
	@cd $(TEST_LIBS) && ./holide.sh            fullclean
	@cd $(TEST_LIBS) && ./verine.sh            fullclean
	@cd $(TEST_LIBS) && ./iprover.sh           fullclean
	@cd $(TEST_LIBS) && ./dklib.sh             fullclean
	@cd $(TEST_LIBS) && ./zenon_modulo.sh      fullclean
	@cd $(TEST_LIBS) && ./dedukti-libraries.sh fullclean

#### Cleaning targets ########################################################

.PHONY: distclean
distclean: clean cleanlibs
	$(Q)find -name "*~" -exec rm {} \;
	$(Q)find -name "*.dko" -exec rm {} \;
	$(Q)rm -f kernel/version.ml
	$(Q)rm -f META

.PHONY: fullclean
fullclean: distclean fullcleanlibs

.PHONY: bnf
bnf:
	@echo "<ident> ::= [a-zA-Z0-9_!?] [a-zA-Z0-9_!?']*"
	@echo "          | '{|' <string> '|}'"
	@echo ""
	@echo "<mident> ::= [a-zA-Z0-9_]*"
	@echo ""
	@echo "<qident> ::= <mident> '.' <ident>"
	@echo ""
	@obelisk parser/menhir_parser.mly | sed "s/ COLON / ':' /g ; s/ RIGHTPAR/ ')'/g ; s/ FATARROW / '=>' /g ; s/ DEF / ':=' /g ; s/ LEFTPAR / '(' /g ; s/ ARROW / '->' /g ; s/ ID/ <ident>/g ; s/ TYPE/ 'Type'/g; s/ QID/ <qident>/g ; s/ LEFTBRA / '{' /g ; s/ RIGHTBRA/ '}' /g ; s/ UNDERSCORE/ '_'/g ; s/COMMA/','/g ; s/ LONGARROW / '-->' /g ; s/ LEFTSQU / '[' /g ; s/ RIGHTSQU/ ']'/g ; s/ DOT/ '.'/g ; s/KW_DEF/'def'/g ; s/KW_THM/'thm'/g ; s/ EVAL / '#EVAL' /g ; s/ INFER / '#INFER' /g ; s/ CHECK / '#CHECK' /g ; s/ CHECKNOT / '#CHECKNOT' /g ; s/ ASSERT / '#ASSERT' /g ; s/ ASSERTNOT / '#ASSERTNOT' /g ; s/ PRINT / '#PRINT' /g ; s/ GDT / '#GDT' /g ; s/ REQUIRE / '#REQUIRE' /g ; s/ NAME / '#NAME' /g ; s/ EQUAL / '=' /g ; s/ STRING / '\"' <string> '\"' /g"
