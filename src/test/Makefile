# Makefile for actorscript test cases

# This works with bash

ASC = ../asc
WASM = ../wasm
ASC_FLAGS = -t -v
OKDIR = ok
OUTDIR = _out

TESTS = $(basename $(wildcard *.as))

SHELL = /bin/bash

OUTTESTS = $(addprefix $(OUTDIR)/, $(TESTS))
SUFFIXES = .tc .run .wat .wat.stderr .wat-run
GENERATED = \
  $(addsuffix .tc, $(OUTTESTS)) \
  $(addsuffix .run, $(OUTTESTS)) \
  $(addsuffix .wat, $(OUTTESTS)) \
  $(addsuffix .wat.stderr, $(OUTTESTS)) \
  $(addsuffix .wat-run, $(OUTTESTS))

bla:
	echo $(addsuffix .a .b, x y)

all: stats

$(OUTDIR):
	mkdir -p $@

$(OUTDIR)/%.tc: %.as $(ASC) Makefile
	$(ASC) --typecheck-only $< > $@ 2>&1 || true

$(OUTDIR)/%.run: %.as $(OUTDIR)/%.tc
	if [ -s $*.tc ]; \
	then echo "Typecheck failed"; touch $@; \
	else $(ASC) -r -v $< > $@ 2>&1 || true; \
	fi

$(OUTDIR)/%.wat $(OUTDIR)/%.wat.stderr: %.as $(OUTDIR)/%.tc
	if [ -s $*.tc ]; \
	then echo "Typecheck failed"; touch $@; touch $@.stderr; \
	else $(ASC) -c $< > $@ 2> $@.stderr || true; \
	fi

$(OUTDIR)/%.wat-run: $(OUTDIR)/%.wat
	if [ -s $< ];\
	then $(WASM) $< > $@ 2>&1 || true; \
	else echo "Empty .wat file!"; touch $@; \
	fi

$(OKDIR)/%.ok:
	@echo "$@ missing"
	@echo "Please run 'make accept' or 'make $*.refresh'"
	@exit 1

$(OUTDIR)/%.diff: $(OKDIR)/%.ok $(OUTDIR)/%
	@diff -u $(OKDIR)/$*.ok $(OUTDIR)/$* | tee $@ || true

%.refresh: $(OUTDIR)/%
	cat $(OUTDIR)/$* >$(OKDIR)/$*.ok

%.refresh-all: $(addprefix %,$(addsuffix .refresh,$(SUFFIXES)))

%.wasm: %.wat
	$(WASM) -d $< -o $@ || true
	touch $@

stats: $(OUTDIR) $(addsuffix .diff,$(GENERATED))
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
	rm -rf $(OUTDIR)

.PHONY: stats current accept all

FORCE:

.PRECIOUS: $(GENERATED)

