.PHONY: all
all: theories example nat solution meta universo

CHECK = dk check

META = dk meta

PRUNE = dk prune

UNIVERSO = universo

example/output:
	mkdir example/output

meta/input:
	mkdir meta/input

meta/output:
	mkdir meta/output

universo/input:
	mkdir universo/input

universo/output:
	mkdir universo/output

universo/raw_output:
	mkdir universo/raw_output

theories/%.dko: theories/%.dk
	$(CHECK) -e $<

# Clean up all the generated files
.PHONY: clean
clean:
	find . -name "*.dko" -exec rm -rf {} \; || true
	rm -rf example/output || true
	rm -rf meta/input || true
	rm meta/prune/eq.dk || true
	rm -rf meta/output || true
	rm -rf universo/input || true
	rm -rf universo/raw_output || true
	rm -rf universo/output || true

.PHONY: theories
theories: theories/cts.dko theories/sttfa.dko

.PHONY: example
example: example/eq.dk theories 
	$(CHECK) -I theories -e $<

.PHONY: nat
nat: example/nat.dk solution theories
	$(CHECK) -I theories -I example/solution -e $<

.PHONY: solution
solution: example/solution/eq.dk example/output theories
	$(CHECK) -I theories -e $<
	cp example/solution/eq.dk example/output/eq.dk

example/output/eq.dk: example/output example/eq.dk
	cp example/eq.dk example/output/eq.dk

meta/input/eq.dk: example/output/eq.dk meta/input
	cp example/output/eq.dk meta/input/eq.dk

# Using [dk dkmeta] is not user friendly yet and several commands need to be done
# 1. First we need to typecheck the input file
# 2. We can apply dk meta on the input file.
# 3. We prune the output to remove the definition we have introduced for convenience
# 4. We type check the output to ensure it is a valid dedukti file
.PHONY: meta
meta: meta/output/eq.dko

meta/output/eq.dko: meta/output/eq.dk theories
	$(CHECK) -I theories -e meta/output/eq.dk

meta/output/eq.dk: meta/input/eq.dk meta/output theories	
	$(CHECK) -I theories -e meta/input/eq.dk
	$(META) -I meta/input -I theories -m meta/meta.dk meta/input/eq.dk > meta/prune/eq.dk	
	$(PRUNE) -I meta/prune meta/prune/config.dk -o meta/output/

universo/input/eq.dk: universo/input meta/output/eq.dko
	cp meta/output/eq.dk universo/input/eq.dk

universo/output/eq.dk: universo/input/eq.dk universo/output universo/raw_output theories
	$(UNIVERSO) --config universo/universo_cfg.dk --theory theories/cts.dk -I theories -o universo/raw_output/ universo/input/eq.dk
	$(UNIVERSO) -q --config universo/universo_cfg.dk --theory theories/cts.dk -I theories -o universo/raw_output/ universo/input/eq.dk --simplify universo/output

universo/output/eq.dko: universo/output/eq.dk
	$(CHECK) -I theories -e $<

.PHONY: universo
universo: universo/output/eq.dko
