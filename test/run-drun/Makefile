RUNFLAGS = -d

all:
	../run.sh $(RUNFLAGS) *.mo *.drun

accept:
	../run.sh $(RUNFLAGS) -a *.mo *.drun

parallel: $(patsubst %.mo,_out/%_done,$(wildcard *.mo)) $(patsubst %.drun,_out/%_done,$(wildcard *.drun))

clean:
	rm -rf _out

include ../*.mk

# Override the .mo rule from quick.mk to use our RUNFLAGS
_out/%_done: %.mo $(wildcard ../../src/moc) ../run.sh | _out
	@+ chronic ../run.sh $(RUNFLAGS) $<
	@+ touch $@

# Add .drun rule (not in quick.mk)
_out/%_done: %.drun $(wildcard ../../src/moc) ../run.sh | _out
	@+ chronic ../run.sh $(RUNFLAGS) $<
	@+ touch $@
