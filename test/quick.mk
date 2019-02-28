# Note: this rule collection is included for one level deeper

TO-TEST = $(patsubst %.as,_out/%.done,$(wildcard *.as))

.PHONY: quick

quick: $(TO-TEST)

_out:
	@ mkdir -p $@

# run single test, e.g. make _out/AST-56.done
.SECONDEXPANSION:
_out/%.done: %.as $$(wildcard $(ASC)) ../run.sh  | _out
	@ (../run.sh $(RUNFLAGS) $< > $@.tmp && mv $@.tmp $@) || (cat $@.tmp; rm -f $@.tmp; false)
