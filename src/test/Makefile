# Makefile for actorscript test cases

# This works with bash

ASC=../asc
WASM=wasm
ASC_FLAGS=-t -v

TESTS:=$(basename $(wildcard *.as))

all: stats

SHELL=/bin/bash

%.out : %.as $(ASC) Makefile
	@echo "TEST: $*"
# Show output when this is an explicit target, otherwise be silent
	@if [ "$(MAKECMDGOALS)" = "$@" ]; \
	then exec > >(tee $@); \
	else exec > $@; \
	fi; \
	exec 2>&1; \
	\
	echo "Interpreter:"; \
	$(ASC) -r -v $(ASC_FLAGS) $< ; \
	echo "Compiler:"; \
	$(ASC) -c $< > $*.wat; \
	cat $*.wat; \
	if [ -s $*.wat ];\
	  then echo "Interpreting the .wat:"; \
	  $(WASM) $*.wat ; \
	  else echo "Empty .wat file!"; \
	fi; \
	true

%.ok:
	@echo "$@ missing"
	@echo "Please run 'make accept' or 'make $*.refresh'"
	@exit 1

%.diff: %.ok %.out
	@diff -u $*.ok $*.out | tee $@ || true

%.refresh: %.out
	cat $*.out >$*.ok

%.wat: %.as
	$(ASC) -c $< > $@

%.wasm: %.wat
	$(WASM) -d $< -o $@

stats: $(addsuffix .diff,$(TESTS))
	@failed=""; \
	for test in $(TESTS); do \
	  if [ -s $$test.diff ]; then \
	    failed="$$failed $$test"; \
	  fi; \
	done; \
	if [ -z "$$failed" ]; \
	then echo "All passed!"; \
	else echo "Failed tests:$$failed"; exit 1; \
	fi

# refresh all
current: $(addsuffix .refresh, $(TESTS))
accept: current

clean:
	rm -f $(addsuffix .diff, $(TESTS)) $(addsuffix .out, $(TESTS))

.PHONY: stats current accept all # $(addsuffix .refresh, $(TESTS))

FORCE:

.PRECIOUS: %.out
