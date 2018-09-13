# Makefile for actorscript test cases

# This works with bash

ASC = ../asc
WASM = wasm
ASC_FLAGS = -t -v
OKDIR = ok
OUTDIR = _out

TESTS = $(basename $(wildcard *.as))

SHELL = /bin/bash

SUFFIXES = .tc .run .wat .wat.stderr .wat-run

GENERATED = \
  $(addsuffix .tc, $(TESTS)) \
  $(addsuffix .run, $(TESTS)) \
  $(addsuffix .wat, $(TESTS)) \
  $(addsuffix .wat.stderr, $(TESTS)) \
  $(addsuffix .wat-run, $(TESTS)) \

GENERATED_OUT = $(addprefix $(OUTDIR)/, $(GENERATED))

all: stats

$(OUTDIR):
	mkdir -p $@

$(OUTDIR)/%.tc: %.as $(ASC) Makefile
	mkdir -p $(OUTDIR); $(ASC) --typecheck-only $< > $@ 2>&1 || true

$(OUTDIR)/%.run: %.as $(OUTDIR)/%.tc
	@if [ -s $*.tc ]; \
	then echo "Typecheck failed"; touch $@; \
	else $(ASC) -r -v $< > $@ 2>&1 || true; \
	fi

$(OUTDIR)/%.wat $(OUTDIR)/%.wat.stderr: %.as $(OUTDIR)/%.tc
	@if [ -s $*.tc ]; \
	then echo "Typecheck failed"; touch $@; touch $@.stderr; \
	else $(ASC) -c $< > $@ 2> $@.stderr || true; \
		echo Hack until opam wasm package is updated >/dev/null; \
		mv $@ $@.tmp; \
		sed -E 's/call_indirect ([$$]?[-_.a-zA-Z0-9]+)/call_indirect (type \1)/g' < $@.tmp > $@; \
		rm $@.tmp; \
	fi

$(OUTDIR)/%.wat-run: $(OUTDIR)/%.wat
	@if [ -s $< ]; \
	then $(WASM) $< > $@ 2>&1 || true; \
	else echo "Empty .wat file $*.wat!"; touch $@; \
	fi

$(OKDIR)/%.ok:
	@echo "$@ missing"
	@echo "Please run 'make accept' or 'make $*.refresh'"
	@exit 1

$(OUTDIR)/%.diff: $(OKDIR)/%.ok $(OUTDIR)/%
	@diff -u $(OKDIR)/$*.ok $(OUTDIR)/$* | tee $@ || true


$(addsuffix .refresh, $(TESTS)) : %.refresh: $(addsuffix .refresh, $(addprefix %, $(SUFFIXES)))

$(addsuffix .refresh, $(GENERATED)) : %.refresh : $(OUTDIR)/%
	mkdir -p $(OKDIR) && cp $< $(OKDIR)/$*.ok

%.wasm: %.wat
	$(WASM) -d $< -o $@ || true
	touch $@

stats: $(OUTDIR) $(addsuffix .diff,$(GENERATED_OUT))
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

.PHONY: stats current accept all %.refresh

FORCE:

.PRECIOUS: $(GENERATED_OUT)

