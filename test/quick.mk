# Note: this rule collection is included from one level deeper

TO-TEST = \
  $(patsubst %.as,_out/%.done,$(wildcard *.as)) \
  $(patsubst %.sh,_out/%.done,$(wildcard *.sh)) \
  $(patsubst %.wat,_out/%.done,$(wildcard *.wat)) \
  $(patsubst %.did,_out/%.done,$(wildcard *.did)) \


.PHONY: quick

quick: $(TO-TEST)

_out:
	@ mkdir -p $@

# run single test, e.g. make _out/AST-56.done
_out/%.done: %.as $(wildcard ../../src/asc) ../run.sh  | _out
	@+ (../run.sh $(RUNFLAGS) $< > $@.tmp && mv $@.tmp $@) || (cat $@.tmp; rm -f $@.tmp; false)
_out/%.done: %.sh $(wildcard ../../src/asc) ../run.sh  | _out
	@+ (../run.sh $(RUNFLAGS) $< > $@.tmp && mv $@.tmp $@) || (cat $@.tmp; rm -f $@.tmp; false)
_out/%.done: %.wat $(wildcard ../../src/asc) ../run.sh  | _out
	@+ (../run.sh $(RUNFLAGS) $< > $@.tmp && mv $@.tmp $@) || (cat $@.tmp; rm -f $@.tmp; false)
_out/%.done: %.did $(wildcard ../../src/didc) ../run.sh  | _out
	@+ (../run.sh -n $(RUNFLAGS) $< > $@.tmp && mv $@.tmp $@) || (cat $@.tmp; rm -f $@.tmp; false)
