# Note: this rule collection is included for one level deeper

TO-TEST = $(patsubst %.as,_out/%.done,$(wildcard *.as))

quick: $(TO-TEST)

# run single test, e.g. make _out/AST-56.done
_out/%.done: %.as $(ASC) ../run.sh
	@ mkdir -p _out
	@ (../run.sh $(RUNFLAGS) $< > $@.tmp && mv $@.tmp $@) || (cat $@.tmp; rm -f $@.tmp; false)
