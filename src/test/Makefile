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
  $(sort \
  $(addsuffix .tc, $(TESTS)) \
  $(addsuffix .run, $(TESTS)) \
  $(addsuffix .wat, $(TESTS)) \
  $(addsuffix .wat.stderr, $(TESTS)) \
  $(addsuffix .wat-run, $(TESTS)) \
  )

GENERATED_OUT = $(addprefix $(OUTDIR)/, $(GENERATED))

all: stats

$(OUTDIR):
	mkdir -p $@

$(OUTDIR)/%.tc: %.as $(ASC) Makefile
	@echo -n [check] $*
	@mkdir -p $(OUTDIR); $(ASC) --check $< > $@ 2>&1 || true
	@if [ -s $@ ];  then echo " ✗"; else echo " ✓"; fi

$(OUTDIR)/%.run: %.as $(OUTDIR)/%.tc
	@if [ -s _out/$*.tc ]; \
	then > $@; \
	else \
	  echo "[run]   $*"; \
	  $(ASC) -r -v $< > $@ 2>&1 || true; \
	fi

$(OUTDIR)/%.wat $(OUTDIR)/%.wat.stderr: %.as $(OUTDIR)/%.tc
	@if [ -s _out/$*.tc ]; \
	then > $(OUTDIR)/$*.wat; > $(OUTDIR)/$*.wat.stderr; \
	else \
          echo "[comp]  $*"; \
	  $(ASC) -c $< > $(OUTDIR)/$*.wat 2> $(OUTDIR)/$*.wat.stderr || true; \
	  echo Hack until opam wasm package is updated >/dev/null; \
	  mv $(OUTDIR)/$*.wat $(OUTDIR)/$*.wat.tmp ; \
          sed -E 's/call_indirect ([$$]?[-_.a-zA-Z0-9]+)/call_indirect (type \1)/g' < $(OUTDIR)/$*.wat.tmp > $(OUTDIR)/$*.wat; \
	  rm $(OUTDIR)/$*.wat.tmp; \
	fi

$(OUTDIR)/%.wat-run: $(OUTDIR)/%.wat
	@if [ -s $< ]; \
	then \
	 echo "[exec]  $*";\
	 $(WASM) $< > $@ 2>&1 || true; \
	else > $@; \
	fi

$(OKDIR)/%.ok:
	@echo "$@ missing"
	@echo "Please run 'make accept' or 'make $*.refresh'"
	@exit 1

$(OUTDIR)/%.diff: $(OKDIR)/%.ok $(OUTDIR)/%
	@diff -u $(OKDIR)/$*.ok $(OUTDIR)/$* | tee $@ || true


$(addsuffix .refresh, $(TESTS)) : %.refresh: $(addsuffix .refresh, $(addprefix %, $(SUFFIXES)))

$(addsuffix .refresh, $(GENERATED)) : %.refresh : $(OUTDIR)/%
	@mkdir -p $(OKDIR) && cp $< $(OKDIR)/$*.ok

%.wasm: %.wat
	$(WASM) -d $< -o $@ || true
	touch $@

stats: $(addsuffix .diff,$(GENERATED_OUT))
	@failed=""; \
	for test in $(GENERATED); do \
	  if [ -s $(OUTDIR)/$$test.diff ]; then \
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

js:
	NODE_PATH=../ node node-test.js

clean:
	rm -rf $(OUTDIR)

.PHONY: stats current accept all %.refresh

FORCE:

.PRECIOUS: $(GENERATED_OUT)


