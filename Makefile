# Current version number of Dedukti.
VERSION = devel

# Compile with "make Q=" to display the commands that are run.
Q = @

.PHONY: default
default: bin binaries

.PHONY: all
all: bin binaries universo.native

.PHONY: binaries
binaries: dk.native

.PHONY: universo
universo:
	$(Q)dune build @install

%.native:
	$(Q)ln -fs _build/install/default/bin/$* $@

universo.native: universo
	$(Q)ln -fs _build/install/default/bin/universo $@


.PHONY: bin
bin: kernel/version.ml
	$(Q)dune build --only-package dedukti @install

.PHONY: doc
doc:
	$(Q)dune build @doc

.PHONY: fmt
fmt:
	$(Q)dune build @fmt

.PHONY: clean
clean:
	$(Q)dune clean
	$(Q)rm -f *.native

.PHONY: install
install: all
	$(Q)dune build @install
	$(Q)dune install

.PHONY: uninstall
uninstall: all
	$(Q)dune uninstall

kernel/version.ml: Makefile
	$(Q)echo 'let version = "$(VERSION)"' > $@


#### Test targets ############################################################

.PHONY: tezt
tezt: bin binaries universo.native
	dune exec tests/main.exe

#### Library tests ###########################################################

TEST_LIBS=libraries/big-libraries

.PHONY: matita
matita: all
	$(Q)echo "## Compiling the Matita's arithmetic library ##"
	$(Q)cd $(TEST_LIBS) && ./matita.sh

.PHONY: matita-light
matita-light: all
	$(Q)echo "## Compiling the Matita's arithmetic library (light) ##"
	$(Q)cd $(TEST_LIBS) && ./matita-light.sh

.PHONY: plein_de_dks
plein_de_dks: all
	$(Q)echo "## Compiling “plein de dks” ##"
	$(Q)cd $(TEST_LIBS) && ./plein_de_dks.sh

.PHONY: focalide
focalide: all
	$(Q)echo "## Compiling focalide library ##"
	$(Q)cd $(TEST_LIBS) && ./focalide.sh

.PHONY: holide
holide: all
	$(Q)echo "## Compiling holide library ##"
	$(Q)cd $(TEST_LIBS) && ./holide.sh

.PHONY: dedukti-libraries
dedukti-libraries: all
	$(Q)echo "## Compiling the Dedukti Libraries folder ##"
	$(Q)cd $(TEST_LIBS) && ./dedukti-libraries.sh

.PHONY: verine
verine: all
	$(Q)echo "## Compiling verine library ##"
	$(Q)cd $(TEST_LIBS) && ./verine.sh

.PHONY: iprover
iprover: all
	$(Q)echo "## Compiling iProverModulo library ##"
	$(Q)cd $(TEST_LIBS) && ./iprover.sh

.PHONY: dklib
dklib: all
	$(Q)echo "## Compiling the dklib library ##"
	$(Q)cd $(TEST_LIBS) && ./dklib.sh

.PHONY: zenon_modulo
zenon_modulo: all
	$(Q)echo "## Compiling the zenon library ##"
	$(Q)cd $(TEST_LIBS) && ./zenon_modulo.sh


.PHONY: light_tests
light_tests: all matita-light dklib focalide

.PHONY: full_tests
full_tests: all dklib focalide matita iprover holide dedukti-libraries verine # zenon_modulo

.PHONY: cleanlibs
cleanlibs:
	$(Q)cd $(TEST_LIBS) && ./matita.sh            clean
	$(Q)cd $(TEST_LIBS) && ./matita-light.sh      clean
	$(Q)cd $(TEST_LIBS) && ./plein_de_dks.sh      clean
	$(Q)cd $(TEST_LIBS) && ./focalide.sh          clean
	$(Q)cd $(TEST_LIBS) && ./holide.sh            clean
	$(Q)cd $(TEST_LIBS) && ./verine.sh            clean
	$(Q)cd $(TEST_LIBS) && ./iprover.sh           clean
	$(Q)cd $(TEST_LIBS) && ./dklib.sh             clean
	$(Q)cd $(TEST_LIBS) && ./zenon_modulo.sh      clean
	$(Q)cd $(TEST_LIBS) && ./dedukti-libraries.sh clean

.PHONY: fullcleanlibs
fullcleanlibs:
	$(Q)cd $(TEST_LIBS) && ./matita.sh            fullclean
	$(Q)cd $(TEST_LIBS) && ./matita-light.sh      fullclean
	$(Q)cd $(TEST_LIBS) && ./plein_de_dks.sh      fullclean
	$(Q)cd $(TEST_LIBS) && ./focalide.sh          fullclean
	$(Q)cd $(TEST_LIBS) && ./holide.sh            fullclean
	$(Q)cd $(TEST_LIBS) && ./verine.sh            fullclean
	$(Q)cd $(TEST_LIBS) && ./iprover.sh           fullclean
	$(Q)cd $(TEST_LIBS) && ./dklib.sh             fullclean
	$(Q)cd $(TEST_LIBS) && ./zenon_modulo.sh      fullclean
	$(Q)cd $(TEST_LIBS) && ./dedukti-libraries.sh fullclean

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
	$(Q)echo "<ident> ::= [a-zA-Z0-9_!?] [a-zA-Z0-9_!?']*"
	$(Q)echo "          | '{|' <string> '|}'"
	$(Q)echo ""
	$(Q)echo "<mident> ::= [a-zA-Z0-9_]*  | '{|' <string> '|}'"
	$(Q)echo ""
	$(Q)echo "<qident> ::= <mident> '.' <ident>"
	$(Q)echo ""
	$(Q)obelisk parsing/menhir_parser.mly | sed "s/ COLON / ':' /g ; s/ RIGHTPAR/ ')'/g ; s/ FATARROW / '=>' /g ; s/ DEF / ':=' /g ; s/ LEFTPAR / '(' /g ; s/ ARROW / '->' /g ; s/ ID/ <ident>/g ; s/ TYPE/ 'Type'/g; s/ QID/ <qident>/g ; s/ LEFTBRA / '{' /g ; s/ RIGHTBRA/ '}' /g ; s/ UNDERSCORE/ '_'/g ; s/COMMA/','/g ; s/ LONGARROW / '-->' /g ; s/ LEFTSQU / '[' /g ; s/ RIGHTSQU/ ']'/g ; s/ DOT/ '.'/g ; s/KW_DEF/'def'/g ; s/KW_INJ/'injective'/g ; s/KW_PRV/'private'/g ; s/KW_THM/'thm'/g ; s/ EVAL / '#EVAL' /g ; s/ INFER / '#INFER' /g ; s/ CHECK / '#CHECK' /g ; s/ CHECKNOT / '#CHECKNOT' /g ; s/ ASSERT / '#ASSERT' /g ; s/ ASSERTNOT / '#ASSERTNOT' /g ; s/ PRINT / '#PRINT' /g ; s/ GDT / '#GDT' /g ; s/ REQUIRE / '#REQUIRE' /g ; s/ NAME / '#NAME' /g ; s/ EQUAL / '=' /g ; s/ STRING / '\"' <string> '\"' /g"
