# Note: this rule collection is included from one level deeper

TO-TEST = \
  $(patsubst %.mo,_out/%_done,$(wildcard *.mo)) \
  $(patsubst %.sh,_out/%_done,$(wildcard *.sh)) \
  $(patsubst %.wat,_out/%_done,$(wildcard *.wat)) \


.PHONY: quick

quick: $(TO-TEST)

_out:
	@ mkdir -p $@

# run single test, e.g. make _out/AST-56_done
# _done, not .done, because run.sh likes to clean $base.*
_out/%_done: %.mo $(wildcard ../../src/moc) ../run.sh  | _out
	@+ chronic ../run.sh $(RUNFLAGS) $<
	@+ touch $@
_out/%_done: %.sh $(wildcard ../../src/moc) ../run.sh  | _out
	@+ chronic ../run.sh $(RUNFLAGS) $<
	@+ touch $@
_out/%_done: %.wat $(wildcard ../../src/moc) ../run.sh  | _out
	@+ chronic ../run.sh $(RUNFLAGS) $<
	@+ touch $@
