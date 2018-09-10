# Makefile for actorscript test cases

# This works with bash

ASC=../asc
WASM=wasm
ASC_FLAGS=-t -v

TESTS:=$(basename $(wildcard *.as))

all: stats

SHELL=/bin/bash

GENERATED := \
  $(addsuffix .tc, $(TESTS)) \
  $(addsuffix .run, $(TESTS)) \
  $(addsuffix .wat, $(TESTS)) \
  $(addsuffix .wat.stderr, $(TESTS)) \
  $(addsuffix .wat-run, $(TESTS))

%.tc : %.as $(ASC) Makefile
	$(ASC) --typecheck-only $< > $@ 2>&1 || true

%.run : %.as %.tc
	if [ -s $*.tc ]; \
	then echo "Typecheck failed"; touch $@; \
	else $(ASC) -r -v $< > $@ 2>&1 || true; \
	fi

%.wat %.wat.stderr : %.as %.tc
	if [ -s $*.tc ]; \
	then echo "Typecheck failed"; touch $@; touch $@.stderr; \
	else $(ASC) -c $< > $@ 2> $@.stderr || true; \
	fi

%.wat-run : %.wat
	if [ -s $< ];\
	then $(WASM) $< > $@ 2>&1 || true; \
	else echo "Empty .wat file!"; touch $@; \
	fi

%.ok:
	@echo "$@ missing"
	@echo "Please run 'make accept' or 'make $*.refresh'"
	@exit 1

%.diff: %.ok %
	@diff -u $*.ok $* | tee $@ || true

%.refresh: %
	cat $* >$*.ok

%.wasm: %.wat
	$(WASM) -d $< -o $@ || true
	touch $@

stats: $(addsuffix .diff,$(GENERATED))
	@failed=""; \
	for test in $(GENERATED); do \
	  if [ -s $$test.diff ]; then \
	    failed="$$failed $$test"; \
	  fi; \
	done; \
	if [ -z "$$failed" ]; \
	then echo "All passed!"; \
	else echo "Failed tests:$$failed"; exit 1; \
	fi

# refresh all
current: $(addsuffix .refresh, $(GENERATED))
accept: current

clean:
	rm -f $(addsuffix .diff, $(GENERATED)) $(GENERATED)

.PHONY: stats current accept all

FORCE:

.PRECIOUS: $(GENERATED)

