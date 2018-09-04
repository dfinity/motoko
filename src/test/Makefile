# Makefile for actorscript test cases

# This works with bash

ASC=../asc
WASM=wasm
ASC_FLAGS=-t -d

TESTS:=$(basename $(wildcard *.as))

all: stats

%.out : %.as $(ASC)
	@echo "TEST: $*"
	@( echo "Interpreter:"; \
	  $(ASC) $(ASC_FLAGS) $< ; \
	  echo "Compiler:"; \
	  $(ASC) -c $< > $*.wat; \
	  cat $*.wat; \
	  if [ -s $*.wat ];\
	    then echo "Interpreting the .wat:"; \
	    $(WASM) $*.wat ; \
	    else echo "Empty .wat file!"; \
	  fi; \
	) > $@ 2>&1 || true

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
	wasm -d $< -o $@

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
