# Makefile for actorscript  cases

# This works with bash

ASC=../asc
ASC_FLAGS=-t -d

TESTS:=$(basename $(wildcard *.as))

all: stats

%.out : %.as $(ASC)
	$(ASC) $(ASC_FLAGS) $< > $@ 2>&1 || true

%.diff : %.out %.ok
	diff -u $*.out $*.ok | tee $@ || true

%.refresh : %.out
	cat $*.out > $*.ok

stats: $(addsuffix .diff,$(TESTS))
	@failed=$$(find $^ -not -empty -printf "%f\n"|sed -e s,\.diff,,); \
	if [ -z "$$failed" ]; \
	then echo "All passed!"; \
	else echo "Failed tests: $$failed"; exit 1; \
	fi

# refresh all
current: $(addsuffix .refresh, $(TESTS))
accept: current

clean:
	rm -f $(addsuffix .diff, $(TESTS)) $(addsuffix .out, $(TESTS))

.PHONY: stats current accept all # $(addsuffix .refresh, $(TESTS))

FORCE:
